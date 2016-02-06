from .wintypes import *
import ctypes

class IncorrectSizeError(Exception):
    def __init__(self, type):
        super().__init__('Incorrect length of data while initializing %s' % type)

class StructureUnionBase(object):
    def __init__(self, basetype, *args, **kwargs):

        from ..fileio import FileStream

        if len(args) == 1 and isinstance(args[0], (bytes, bytearray, FileStream)):
            basetype.__init__(self)
            buf = args[0]
            if isinstance(buf, FileStream):
                length = kwargs.get('Length', len(self))
                buf = buf.Read(length)
                if len(buf) != length:
                    raise IncorrectSizeError(type(self))

            self.__frombytes__(buf)

        else:
            basetype.__init__(self, *args, **kwargs)

    def __bytes__(self):
        size = len(self)
        buff = (CHAR * size)()
        ctypes.memmove(buff, ctypes.addressof(self), size)

        return bytes(buff)

    def __frombytes__(self, buffer):
        size = len(buffer)
        buff = (CHAR * size).from_buffer_copy(buffer)
        ctypes.memmove(ctypes.addressof(self), ctypes.addressof(buff), size)

        return self

    def __len__(self):
        return ctypes.sizeof(self)

class Structure(ctypes.Structure, StructureUnionBase):
    def __init__(self, *args, **kwargs):
        StructureUnionBase.__init__(self, ctypes.Structure, *args, **kwargs)

class Union(ctypes.Union, StructureUnionBase):
    def __init__(self, *args, **kwargs):
        StructureUnionBase.__init__(self, ctypes.Union, *args, **kwargs)
