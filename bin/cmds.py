from ml import *
import lldb

def lldb_clear(debugger, command, result, internal_dict):
    console.clear()

def __lldb_init_module(debugger, internal_dict):
    self = __name__
    debugger.HandleCommand('command script add -f %s.lldb_clear cls' % self)
