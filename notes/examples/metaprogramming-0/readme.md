This example is a re-write of an input handling system I wrote in C++ for a
game.

The input is done with GLFW, and is a singleton that tracks the state of
'controls' where a control maps some in-game semantic control to a GLFW button.

GLFW is assuming to be stored in a 'glfw' module, where everything is stored top level and properly renamed. For example:

```c
glfwInit();
GLFWWindow* window = glfwCreateWindow(...);
glfwMakeContextCurrent(window);
```

```cello
glfw::init();
var window = glfw::createWindow(...);
glfw::makeContextCurrent(window);
```

