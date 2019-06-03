import copy
from llvmlite import ir

class Var:
    ## @param var_type - See the 'lang_type' module
    ## @param val - The LLVM value that represents this variable. None if this
    ## is a kind. This is a python value if is_comptime.

    def __init__(self, var_type, val=None, is_mutable=False, is_member=False, is_comptime=False, name=""):
        self.name = name
        self.var_type = var_type
        self.val = val
        self.is_comptime = is_comptime
        self.is_mutable = is_mutable
        self.is_member = is_member

    def clone(self):
        return copy.deepcopy(self)

## Class for managing scope.
class Scope:
    ## @param is_comptime - Are we in a comptime scope?
    def __init__(self, parent=None, is_comptime=False):
        self.parent = parent
        ## A map of strings to Vars
        self.symbol_table = {}
        self.is_comptime = False

    def comptime_subscope(self):
        return Scope(parent=self, is_comptime=True)

    def subscope(self):
        return Scope(parent=self)

    def set(self, name, var: Var): self.symbol_table[name] = var

    def print_scope(self):
        if self.parent: self.parent.print_scope()
        print(self.symbol_table)

    ## Looks up a value - if the value is comptime, does nothing to the value,
    ## meaning var.val will be a python value (NOT an llvm value). You probably
    ## always want to use lookup(), unless writing an interface to the JIT
    ## where you need exact references into the symbol table.
    def lookup_exact(self, name, is_comptime=None):
        if is_comptime == None: is_comptime = self.is_comptime
        local = self.symbol_table.get(name)
        if is_comptime and not local.is_comptime: return None
        if local == None:
            ## If we can't find the symbol here, look in parents
            if self.parent == None: return None
            else: return self.parent.lookup_exact(name, is_comptime)
        else:
            return local

    ## NOTE: Values returned from this are not necessarily the values in the
    ## symbol table. To mutate variables, always use set(), rather than just
    ## mutating the result of this function, and never mutate the result of
    ## this function assuming it's not present in the symbol table.
    ## Use lookup_exact for exact symbol table values.
    def lookup(self, name, is_comptime=None):
        local = self.lookup_exact(name, is_comptime)
        if not local: return None
        if local.is_comptime and not is_comptime:
            ## If the var is comptime but we're not in a comptime context,
            ## then the value is a python value. We need to create an LLVM
            ## constant and return that.
            cloned = local.clone()
            cloned.val = ir.Constant(local.var_type.to_llvm_type(), local.val)
            return cloned
        else:
            return local
