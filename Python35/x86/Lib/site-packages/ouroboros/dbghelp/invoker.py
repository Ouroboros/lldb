import traceback
import sys

def Try(method, *values):
    try:
        return method(*values)
    except Exception as e:
        from .. import console
        traceback.print_exception(*sys.exc_info())
        console.pause()

def DbgTry(method, *values):
    try:
        return method(*values)
    except Exception as e:
        from . import breakpoint
        traceback.print_exception(*sys.exc_info())
        bp()

TryInvoke = Try
