def bp():
    from pdb import set_trace as p_bp
    bp = p_bp
    return bp()

def ibp_init():
    global ibp
    try:
        from ipdb import set_trace as ibp
    except:
        ibp = ibp_stub

    return ibp()

def ibp_stub():
    bp()

class qtbp(object):
    def __init__(self):
        from PyQt5.QtCore import pyqtRemoveInputHook
        pyqtRemoveInputHook()
        ibp()

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        from PyQt5.QtCore import pyqtRestoreInputHook
        pyqtRestoreInputHook()


ibp = ibp_init