from typing import List
from lang_type import *

# Given a base name, list of type param concrete types, list of param types, mangle the function accordingly
def mangle_function(base_name: str, type_params: List[Type]):
    for t in type_params: base_name += t.mangle()
    return base_name

curr_global_num = 0
def mangle_global():
    global curr_global_num
    num = curr_global_num
    curr_global_num += 1
    return "__global_" + str(num)
