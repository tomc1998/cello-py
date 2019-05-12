class Var:
    ## @param var_type - See the 'lang_type' module
    ## @param val - The LLVM value that represents this variable. None if this is a kind.
    def __init__(self, var_type, val=None, is_mutable=False, is_member=False, name=""):
        self.name = name
        self.var_type = var_type
        self.val = val
        self.is_mutable = is_mutable
        self.is_member = is_member

## Class for managing scope.
class Scope:
    def __init__(self, parent=None):
        self.parent = parent
        ## A map of strings to Vars
        self.symbol_table = {}

    def subscope(self):
        return Scope(self)

    def set(self, name, var: Var): self.symbol_table[name] = var

    def lookup(self, name):
        local = self.symbol_table.get(name)
        if local == None:
            ## If we can't find the symbol here, look in parents
            if self.parent == None: return None
            else: return self.parent.lookup(name)
        else:
            return local
