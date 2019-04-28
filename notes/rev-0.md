# Cello 2

Second iteration on cello, low-level language focused on compile time metaprogramming

## Required features

### Compile-time code generation

A lisp-esque macro system, with easy splicing.

### Compile-time state and include vs import

Macros can alter global compile-time state, to alter the next time macros run

To make this possible, code execution must be in a fixed order

All top-level static types are parsed ahead of time, and function names &
signatures, in a pre-pass. The modules are processed in a defined order. One
modules compiles the whole project, with that modules including other modules. Each
modules can include any other modules, and a modules cannot be included twice.

All modules will declare the types / functions they declare ahead of time. This way we can
have circular imports (even if includes need to be done in the right order).
This means we can declare functions that take pointers to types which haven't
been finalized, and also forward declare functions in general.

A second pass processes all compile-time code generation / metaprogramming,
and jits all macros. Macros will have access to different levels of
introspection information depending on whether a file has already been
processed or not. Example:

```
We have three files, main.cel, foo.cel, and bar.cel. Foo imports bar, but is included before bar.cel:

/*******************************/
/*          main.cel           */
/*******************************/

#include foo
#include bar


/*******************************/
/*           bar.cel           */
/*******************************/

// A normal struct decl
export type MyStaticStruct = struct {
  a: i32
  b: i32
};

// A struct decl requiring some processing
export type MyDynamicStruct = comptime {
  var struct = meta::struct();
  struct.fields = [
    ("c", i32),
    ("d", i32),
  ];
  struct
};

/*******************************/
/*           foo.cel           */
/*******************************/

#import foo

// Here, we declare an array of ints which is the size of foo::MyStaticStruct
type MyArrayType = comptime {
  var array_type = meta::array();
  var static_struct_type = foo::MyStaticStruct;
  array_type.len = static_struct_type.fields?.len;
  array_type.type = i32;
  array_type
}
var a : MyArrayType; // type i32[2];

// Try and do the same thing with MyDynamicStruct:
type MyArrayType = comptime {
  var array_type = meta::array();
  var dynamic_struct_type = foo::MyStaticStruct;
  // Compile error: dynamic struct type not finalised, so can't access 'fields'.
  array_type.len = dynamic_struct_type.fields?.len;

  ...
}
```

Here, we get a compile error trying to access `fields` on
`dynamic_struct_type`, because fields is not initialised yet (specifically,
it's 'none', see the 'Optional types' feature).

This is because in main.cel we `#include` foo before bar. To resolve this
issue, swap the includes around - this makes bar fully evaluate before foo,
meaning that `MyStaticStruct.fields` is initialised by the time foo runs.

including is NOT the same as importing. A modules can be imported multiple
times, but only included once. 'Inclusion' indicates that a modules is
processed - 'importing' is just a way to import names into the modules.

### Modules

Modules are organised like rust - either a file or folder containing a 'mod.cel'
file is considered a module.

Modules are nested, with mod.cel responsible for #including all the other
modules in that given folder.

#includes can ONLY happen in main.cel, and mod.cel (the main and root-module
files).

ALL module files in the folder must be included - if a file wants to be
ignored, it must be `#ignore`d - e.g. `#ignore foo`. This is to help with
maintaining module ordering.

## Resolving circular templated types

This was an issue in terra (see https://github.com/zdevito/terra/issues/316). We
can template structures with comptime macros:

```
comptime fn Vec = (T: $type) -> $type {
  `struct {
    ptr: *@T,
    len: i32,
    cap: i32,
  }
};

var v0 : Vec(i32) = { ... };
var v1 : Vec(f32) = { ... };
```

However, this becomes annoying if we have a circular type. Consider the following example:
```
type Foo = struct {
  b: &Bar,
};

type Bar = struct {
  f: Foo,
};
```

This won't work, because Foo requires Bar before it's been read.

To fix this, we can do a pre-pass to just parse the names of structs, THEN parse
the structures.

However, this isn't so simple with comptime types, where we can't do this pre-pass:

```
comptime fn Foo = (T: $type) -> $type {
  `struct {
    b: &@Bar(T)
  }
};

comptime fn Bar = (T: $type) -> $type {
  `struct {
    f: @Foo(T),
    val: T
  }
};
```

This results in problems, because in order to construct Foo(T) we need Bar(T),
and in order to construct that we need Foo(T), which hasn't returned yet ...
hence infinite recursion.

Let's see how this would work in C++:

```c++
template <typename T>
struct Bar;

template <typename T>
struct Foo {
  Bar<T>* b;
};

template <typename T>
struct Bar {
  Foo<T> b;
};
```

So, we need a forward declaration, but seems to work okay.

This is a pretty fundamental issue - c++ effectively gives us the ability to
forward declare comptime functions, AND call them without actually having an
implementation, which is hard to do in our more 'general' system. Even if we
could get around it and use a c++ like system, we'd still have problem with the
order of data / code declarations, which c++ solves with forward declarations:

```

comptime fn Foo = (T: $type) -> $type {
  `struct {
    b: &@Bar(T),

		// Even assuming Bar(T) wasn't infinite recursion (which we solved by
		treating it as an undefined type of 'pointer' for now), here we have a
		function which uses the value of b before Bar is actually defined.
    fn do_stuff() -> @T {
        return b->val;
    }
  }
};

comptime fn Bar = (T: $type) -> $type {
  `struct {
    f: @Foo(T),
    val: T
  }
};
```

To solve THIS, we need to make sure that the templating stage ONLY generated
code, and that generated code is then evaluated in 2 passes, with the data
being evaluated first, and the functions evaluated second.

So, we're now back to the original problem, of preventing infinite recursion
(hopefully without introducing the need for forward declarations).

We COULD have ANOTHER pass to resolve pointer types, so Foo(i32) would
initially evaluate to this:

```
struct {
	b: <pointer>
	val: i32
}
```

then once Bar was evaluated, b's type would be complete. this is fine, since
this pass would go after evaluating data, but before evaluating code.

There's another issue with this, though. Modules run in a defined order, since
you want to be able to introspect types from one modules to produce types in
another - for this reason, this 5 pass system must be run one module at a time.
This means that this two way relationship would struggle to exist
cross-modules, since when a module is finished running you'd want the types to
be 100% complete for any further modules.

This exposes a large problem of using incomplete declarations of comptime types:

```
/* foo.cel */

import bar::Bar

comptime fn Foo = (T: $type) -> $type {
	`struct {
		val: &@Bar(T)
	}
}

/* bar.cel */

comptime fn Bar = (T: $type) -> $type {
	`struct {
		val: &@T
	}
}
```

if foo.cel is included before bar.cel, is this valid? Maybe. Bar might depend
on some compile-time data that hasn't been initialised by the module yet,
though. In this case however, it's fine, and hsould be allowed.

This seems to indicate the need for a `needs_include` tag which can prevent
comptime functions from running before a module has been included. Here is an
example of a codebase where that might be needed:

```
comptime var largest_array_size_made = 0;

comptime fn array = (T: $type, x: u64) -> $type {
	largest_array_size_made = math::max(x, largest_array_size_made);
	T[x]
}

// Make some arrays
var arr_0 : array(i32, 4) = undefined;
var arr_2 : array(i32, 6) = undefined;
var arr_1 : array(i32, 5) = undefined;

// last_array_size_made will be 6 here. We're expecting `Bar(T)` to contain an
// array of `T` that can hold an element for each element in the arrays.

comptime fn Bar(T: $type) -> $type {
	`struct {
		values: @T[largest_array_size_made]
	}
}

```

Being the writer of the above module, you would expect `Bar(T)` to be a struct
containing an array of 6 `T`s. However, if we called Bar from another module
before Bar was `#include`d, we'd actually get a struct with an array of size 0.
So, you'd want to limit this to only be runafter this module has run:

```
#needs_include
comptime fn Bar(T: $type) -> $type {
	...
}
```

This means calling `Bar(...)` without having `#include`d the module already
will result in a nice compile error.

## Type inference

It's been mentioned that the way to implement generic types is via macros (i.e.
comptime functions). However, this means we can't infer template parameters,
which is a HUGE drawback.

```
// Maybe we only want to store < 65k entities, so we can use u16 instead of u32 for storing length.
comptime fn Vec = (T: $type, IntType: %type) -> $type {
  `struct {
    ptr: &@T,
    len: @IntType,
    cap: @IntType,
  }
}


// Unfortunately, we can't make this default or infer T.
```

We potentially need to make type-based templating have a specific syntax, so the compiler understands the type inference:

```
type Vec = struct(T: $type, IntType: $type = u16) {
    ptr: &@T,
    len: @IntType,
    cap: @IntType,
    fn copy = (other: &Vec(T)) -> Vec(T) {
        ...
    }
}

var v0 = make Vec(u32) {null, 0, 0};
var v1 = Vec::copy(v0); // Vec::copy infers T based on arg 0
```

Notice same syntax, means we don't have the c++ program of >> operators

We can now access static methods without qualifying the type too, i.e.
`Vec::copy` rather than `Vec(T)::copy`. The (T) can get inferred (see 'Function
type inference' below).

Vec::copy is now `(T: $type) -> ((&Vec(T)) -> Vec(T))`. The special syntax lets
us understand that Vec is a struct with methods, and we can change the types of
functions accordingly.

### Function type inference

Now we can separate the member methods into type functions (i.e. `typeof(Vec::copy)` ==
`comptime fn (T: $type) -> fn (&Vec(T)) -> Vec(T)`), we need a way to use this template to
infer type arguments.

Inference based on arguments is pretty easy, and will probably work similar to
Prolog's 'unification' of types. Obviously if the parameter is just type 'T',
where T is a template parameter, T can unify based on the type of that argument.
It gets harder when the argument is a kind, i.e. Vec(T).

If Vec(T) is a proper 'generic type', i.e. not created with comptime fn, this is
fine. For now, just ignore when Vec is a comptime fn - since that requires more advanced analysis of Vec.

## Example syntax


### Importing and including

```
// Identical syntax to rust

// Include the module 'foo', identified by either 'foo.cel' or 'foo/mod.cel'.
#include foo

// Import the module foo. Values exported can be accessed with foo::FOO.
#import foo

// Import everything inside foo into the global namespace
#import foo::*

// import the symbol 'bar' from foo into the global namespace
#import foo::bar

// Import 'bar' and 'baz' from foo
#import foo::{bar, baz}

// More complex example, with nested modules and aliasing
#import foo::{mod1::*, mod2::bar as mod2_bar, bar}
```

### Declaring values

```
// Declare variable - fairly self explanetory
var x = 3;
var x : i32 = 4;

// Declare a type
type Foo = i32;
type Bar = struct {
  a: i32,
  b: i32,
};

// Declare a function
fn my_fn = (a: i32, b: i32) -> Foo {
}
```

### Comptime expressions (0 parameter macros)

```
// Comptime expressions just generate something, like a function or type.
type Foo = comptime {
  i32;
};

fn my_fn = (a: i32, b: i32) -> Foo {
  // Here, we generate the addition expression at compile time, which is then compiled.
  // This is similar to the lisp syntax, where '`' is a 'quote', and '@' can be
  // used to escape (since ',' is used elsewhere, unlike lisp!).
  return comptime {
    `{a + b}
  };
};

fn my_escape_fn = (a: i32) -> Foo {
  // This is an example of splicing in a value at compile time.
  return comptime {
    `{a + @{math::sqrt(2)}}
  };
};

// When quoting / escaping, normally `{} and @{} is used. This can sometimes be omitted, if what you're quoting / escaping is a single expression:
// `foo = valid
// `(foo + bar) = valid
// `if x { foo } else { bar } = valid
// `x + 3 = invalid, this just evaluates to the value of 'x', then + 3. So, if x was 4, this would evaluate to `{3 + 4}, rather than `7.

// In this example, `my_dyn_fn` is only evaluated when this file
// is `#included`, so the function `my_dyn_fn` CANNOT be used until this is fully
// `#included`. In the case of `my_fn`, the signature is declared statically, so
// this CAN be called before being #included.
fn my_dyn_fn = comptime {
  `{
    (a: i32, b: i32) -> Foo {
      a + b
    }
  };
};
```

### Comptime functions (macros)

```
// Function takes a list of expressions, and returns another expression, adding 
them all together. For example, add_list(3, 4, 5) would return (3) + ((4) + (5)). 
add_list(3, a * b, 7 + 8 + 9) would return (3) + ((a * b) + (7 + 8 + 9)).
comptime fn add_list = (args: $expr...) -> $expr {
  if args.len >= 2 {
    `(@args[0] + @add_list(args[1..]))
  } else if args.len == 0 {
    args[0]
  } else {
    `() // Empty expr
  }
};

// Notice, the macro expressions are sanitized by default, by surrounding with ().

// We can use this to (sort of) template structs. Notice $type instead of
// $expr - this guarantees that the input is a valid syntax for a type.
// Note: if you actually want struct / function templates, use the struct /
// function templates in the 'Real templates' section! These actually support type
// inference and have nicer syntax. This is really just if you have a very complex
// type, and in general won't play too well with type inference / circular
// declarations.

comptime fn MyContainer = (T: $type) -> $type {
  `struct {
    val: *@T
  }
};

type I32Container = MyContainer(i32);
type I64Container = MyContainer(i64);

// Again, notice that this file must be evaluated before I32 can be used anywhere.

// NOTE:
// By default, comptime functions are memoized. This means that calling the 
// function twice with the same input will return EXACTLY the same output, by 
// caching the first result and returning it from the second call. This means 
// that two calls to MyContainer(i32) will ACTUALLY return the same type, not just
// a type that has exactly the same fields. This means you can write functions
// that will accept this type:
fn foo = (v: MyContainer(i32)) {
}

// Then call foo with another var of type MyContainer(i32)
var v : MyContainer(i32) = { val: null };
foo(v);

// This will typecheck, as both MyContainer(i32)s are the same.

// Comptime functions are just functions that can ONLY be called at compile time. 
// Any function can be called at compile time - if called from a comptime function. 
// For example:
fn add = (a: i32, b: i32) -> i32 { return a + b; }
comptime fn array(T: $type, a: i32, b: i32) -> $type {
    T[add(a, b)] // Call an 'add' function, which is not comptime, but will be evaluated at comptime.
}

```

### Structs

```
// Struct with methods
type Adder = struct {
  a: u32,
  b: u32,
  fn add = () -> u32 {
    a + b
  }
}
```

### Real templates (generic types)

```
// comptime fn isn't really suitable for implementing generic types, since we'd 
// have no type inference. This is an example of actual struct and function templates.

type Vec = struct(T: $type, IntType: &type = u16) {
  ptr: &T,
  len: IntType,
  cap: IntType,
  static fn copy = (other: &Vec(T)) -> Vec(T) {
    ...
  }
};

// Here, 'copy' can be inferred when doing stuff like this:
var v0 = make Vec(u32) { null, 0, 0 }
var v1 = Vec::copy(v0); // v1 is a Vec(u32)
// Here, 'copy' has type `comptime fn (T: $type) -> fn (other: &Vec(T)) -> Vec(T)`

// This is a generic function.
fn add(T: $type) = (a: T, b: T) -> T {
  return a + b;
}
// here, `add` has type `comptime fn (T: $type) -> fn (a: T, b: T) -> T`

var x = add(3u32, 4u32); // Inferred that T = u32
var x : i32 = add(3, 4); // Inferred that T = i32
var x = add(i8)(3, 4);   // Explicit typing, x is inferred to be i8
var x = add(3, 4);       // Implicit numbers, x is inferred to be i32 (since 3 and 4 are by deafult i32)
```

### Default int / float types

```
var x = 3;
// what is x?
// x is i32 by default.

var y = 3.4;
// What is y?
// float by default.
var z = 123123123123;
// z is i64.

fn add(T: $type) = (a: T, b: T) -> T { return a + b; }
add(3, 4); // i32
add(3u32, 4); // u32
add(u32)(3, 4); // u32
add(123123123, 123123123); // i64
var x : u8 = add(3, 4); // u8
```
