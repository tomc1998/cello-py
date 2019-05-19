class Var:
    ## @param var_type - See the 'lang_type' module
    ## @param val - The LLVM value that represents this variable. None if this is a kind.
    def __init__(self, var_type, val=None, is_mutable=False, is_member=False, is_comptime=False, name=""):
        self.name = name
        self.var_type = var_type
        self.val = val
        self.is_comptime = is_comptime
        self.is_mutable = is_mutable
        self.is_member = is_member

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

    def lookup(self, name, is_comptime=None):
        if is_comptime == None: is_comptime = self.is_comptime
        local = self.symbol_table.get(name)
        if is_comptime and not local.is_comptime: return None
        if local == None:
            ## If we can't find the symbol here, look in parents
            if self.parent == None: return None
            else: return self.parent.lookup(name, is_comptime)
        else:
            return local
