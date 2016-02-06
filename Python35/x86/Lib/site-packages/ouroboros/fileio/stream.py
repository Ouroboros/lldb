from ..common import *
from ..otypes.wintypes import *
import io

_DEFAULT_ENDIAN = '<'

def _ReadChar(fs, endian = _DEFAULT_ENDIAN):
    return struct.unpack(endian + 'b', fs.read(1))[0]

def _ReadUChar(fs, endian = _DEFAULT_ENDIAN):
    return struct.unpack(endian + 'B', fs.read(1))[0]

def _ReadShort(fs, endian = _DEFAULT_ENDIAN):
    return struct.unpack(endian + 'h', fs.read(2))[0]

def _ReadUShort(fs, endian = _DEFAULT_ENDIAN):
    return struct.unpack(endian + 'H', fs.read(2))[0]

def _ReadLong(fs, endian = _DEFAULT_ENDIAN):
    return struct.unpack(endian + 'l', fs.read(4))[0]

def _ReadULong(fs, endian = _DEFAULT_ENDIAN):
    return struct.unpack(endian + 'L', fs.read(4))[0]

def _ReadLong64(fs, endian = _DEFAULT_ENDIAN):
    return struct.unpack(endian + 'q', fs.read(8))[0]

def _ReadULong64(fs, endian = _DEFAULT_ENDIAN):
    return struct.unpack(endian + 'Q', fs.read(8))[0]

def _ReadFloat(fs, endian = _DEFAULT_ENDIAN):
    return struct.unpack(endian + 'f', fs.read(4))[0]

def _ReadDouble(fs, endian = _DEFAULT_ENDIAN):
    return struct.unpack(endian + 'd', fs.read(8))[0]

def _ReadAString(fs, cp = ANSI_CODE_PAGE):
    string = b''
    while True:
        buf = fs.read(1)
        if buf == b'' or buf == b'\x00':
            break

        string += buf

    return string.decode(cp, errors='ignore')

def _ReadWString(fs):
    string = b''
    while True:
        buf = fs.read(2)
        if buf == b'' or buf == b'\x00\x00':
            break

        string += buf

    return string.decode('U16')

def _WriteByte(fs, b):
    return fs.write(struct.pack('<B', BYTE(int(b) & 0xFF).value))

def _WriteChar(fs, b):
    return fs.write(CHAR(int(b) & 0xFF).value)

def _WriteUShort(fs, ushort, endian = _DEFAULT_ENDIAN):
    return fs.write(struct.pack(endian + 'H', USHORT(int(ushort)).value))

def _WriteShort(fs, short, endian = _DEFAULT_ENDIAN):
    return fs.write(struct.pack(endian + 'h', SHORT(int(short)).value))

def _WriteULong(fs, ulong, endian = _DEFAULT_ENDIAN):
    return fs.write(struct.pack(endian + 'L', ULONG(int(ulong)).value))

def _WriteLong(fs, long, endian = _DEFAULT_ENDIAN):
    return fs.write(struct.pack(endian + 'l', LONG(int(long)).value))

def _WriteLong64(fs, l64, endian = _DEFAULT_ENDIAN):
    return fs.write(struct.pack(endian + 'q', LONG64(int(l64)).value))

def _WriteULong64(fs, ul64, endian = _DEFAULT_ENDIAN):
    return fs.write(struct.pack(endian + 'Q', ULONG64(int(ul64)).value))

def _WriteFloat(fs, flt, endian = _DEFAULT_ENDIAN):
    return fs.write(struct.pack(endian + 'f', FLOAT(float(flt)).value))

def _WriteDouble(fs, db, endian = _DEFAULT_ENDIAN):
    return fs.write(struct.pack(endian + 'd', DOUBLE(float(db)).value))

class FileStreamPositionHolder:
    def __init__(self, fs):
        self.fs = fs

    def __enter__(self):
        self.pos = self.fs.Position
        return self.fs

    def __exit__(self, type, value, traceback):
        self.fs.Position = self.pos

class FileStream(object):
    MemoryFile = False

    LITTLE_ENDIAN = '<'
    BIG_ENDIAN = '>'

    END_OF_FILE = None

    def __init__(self, file = None, mode = 'rb', *, endian = LITTLE_ENDIAN):
        self._stream = None
        self._endian = endian
        self._encoding = ANSI_CODE_PAGE

        if file is not None:
            self.Open(file, mode)

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.Close()

    @property
    def Stream(self):
        return self._stream

    @property
    def Endian(self):
        return self._endian

    @Endian.setter
    def Endian(self, value):
        self._endian = value

    @property
    def Encoding(self):
        return self._encoding

    @Encoding.setter
    def Encoding(self, value):
        self._encoding = value

    def Open(self, file, mode = 'rb'):
        if isinstance(file, (bytes, bytearray)):
            return self.OpenMemory(file)
        elif isinstance(file, type(self)):
            self._stream = file
            self.MemoryFile = file.MemoryFile
            return self
        else:
            return self.OpenFile(file, mode)

    def OpenFile(self, file, mode = 'rb'):
        self._stream = open(file, mode)
        self.MemoryFile = False
        return self

    def OpenMemory(self, buffer = b''):
        self._stream = io.BytesIO(buffer)
        self.MemoryFile = True
        return self

    def Close(self):
        self._stream.close()
        self.MemoryFile = False

    def close(self):
        return self.Close()

    def truncate(self, size):
        return self.Truncate(size)

    def seek(self, offset, method = io.SEEK_SET):
        return self.Seek(offset, method)

    def read(self, n = -1):
        return self.Read(n)

    def write(self, buf):
        return self.Write(buf)

    def tell(self):
        return self.Position

    def flush(self):
        return self.Flush()

    def __getitem__(self, args):
        if isinstance(args, tuple):
            if len(args) == 1:
                offset = args[0]
                size = 1
            elif len(args) == 2:
                offset, size = args
            else:
                raise Exception('invalid parameter number')
        else:
            offset = args
            size = 1

        if offset >= self.Length:
            raise IndexError('offset larger than file size')

        if size == 0:
            return b''

        with FileStreamPositionHolder(self):
            self.Position = offset
            if size == 1:
                b = self.ReadByte()
            elif size == 2:
                b = self.ReadUShort()
            elif size == 4:
                b = self.ReadULong()
            elif size == 8:
                b = self.ReadULong64()
            else:
                b = self.Read(size)

        return b

    def __setitem__(self, args, data):
        if isinstance(args, tuple):
            if len(args) == 1:
                offset = args[0]
                size = 1
            elif len(args) == 2:
                offset, size = args
            else:
                raise Exception('invalid parameter number')
        else:
            offset = args
            size = 1

        if size == 0:
            return

        if isinstance(data, bytes) or isinstance(data, bytearray):
            size = -1

        with FileStreamPositionHolder(self):
            self.Position = offset
            if size == 1:
                b = self.WriteByte(data)
            elif size == 2:
                b = self.WriteUShort(data)
            elif size == 4:
                b = self.WriteULong(data)
            elif size == 8:
                b = self.WriteULong64(data)
            else:
                b = self.Write(data)

        return b

    def __len__(self):
        return self.Length

    @property
    def Length(self):
        return self.GetSize()

    @Length.setter
    def Length(self, length):
        if length == self.Length:
            return

        pos = self.Position

        if length > self.Length:
            self.Position = length
            self.WriteByte(0)

        self.Truncate(length)
        pos = min(pos, length)
        self.Position = pos

    @property
    def Remaining(self):
        return self.Length - self.Position

    @property
    def Position(self):
        return self.GetPosition()

    @Position.setter
    def Position(self, offset):
        if offset == self.END_OF_FILE:
            self.Seek(0, io.SEEK_END)
            return

        #if offset < 0:
        #    self.Seek(offset, io.SEEK_END)
        #else:
        #    print('seek %s' % offset)
        self.Seek(offset, io.SEEK_SET)

    def Truncate(self, size):
        return self._stream.truncate(size)

    def Seek(self, offset, method = io.SEEK_SET):
        return self._stream.seek(offset, method)

    def Read(self, n = -1):
        if not n:
            return b''

        return self._stream.read(n)

    def ReadAll(self):
        self.Position = 0
        return self.Read()

    def Write(self, *bufs):
        written = 0
        for buf in bufs:
            if not buf:
                return

            if isinstance(buf, FileStream):
                with FileStreamPositionHolder(buf) as fs:
                    fs.Position = 0
                    buf = fs.Read()

            written += self._stream.write(buf)

        return written

    def GetPosition(self):
        return self._stream.tell()

    def Flush(self):
        return self._stream.flush()

    def GetSize(self):
        pos = self.Position
        self.Seek(0, io.SEEK_END)
        size = self.GetPosition()
        self.Position = pos
        return size

    def IsEndOfFile(self):
        return self.Position >= self.Length

    def ReadBoolean(self):
        return bool(self.ReadUChar())

    def ReadChar(self):
        return _ReadChar(self._stream, self._endian)

    def ReadUChar(self):
        return _ReadUChar(self._stream, self._endian)

    def ReadByte(self):
        return _ReadUChar(self._stream, self._endian)

    def ReadShort(self):
        return _ReadShort(self._stream, self._endian)

    def ReadUShort(self):
        return _ReadUShort(self._stream, self._endian)

    def ReadLong(self):
        return _ReadLong(self._stream, self._endian)

    def ReadULong(self):
        return _ReadULong(self._stream, self._endian)

    def ReadLong64(self):
        return _ReadLong64(self._stream, self._endian)

    def ReadULong64(self):
        return _ReadULong64(self._stream, self._endian)

    def ReadFloat(self):
        return _ReadFloat(self._stream, self._endian)

    def ReadDouble(self):
        return _ReadDouble(self._stream, self._endian)

    def ReadMultiByte(self, cp = None):
        return _ReadAString(self._stream, cp or self._encoding)

    def ReadUTF16(self):
        return _ReadWString(self._stream)

    def WriteBoolean(self, *values):
        return sum([self.WriteByte(bool(b)) for b in values])

    def WriteChar(self, *values):
        return sum([_WriteChar(self._stream, b) for b in values])

    def WriteByte(self, *values):
        return sum([_WriteByte(self._stream, b) for b in values])

    def WriteShort(self, *values):
        return sum([_WriteShort(self._stream, short, self._endian) for short in values])

    def WriteUShort(self, *values):
        return sum([_WriteUShort(self._stream, ushort, self._endian) for ushort in values])

    def WriteLong(self, *values):
        return sum([_WriteLong(self._stream, l, self._endian) for l in values])

    def WriteULong(self, *values):
        return sum([_WriteULong(self._stream, ul, self._endian) for ul in values])

    def WriteLong64(self, *values):
        return sum([_WriteLong64(self._stream, l64, self._endian) for l64 in values])

    def WriteULong64(self, *values):
        return sum([_WriteULong64(self._stream, ul64, self._endian) for ul64 in values])

    def WriteFloat(self, *values):
        return sum([_WriteFloat(self._stream, flt, self._endian) for flt in values])

    def WriteDouble(self, *values):
        return sum([_WriteDouble(self._stream, db, self._endian) for db in values])

    def WriteMultiByte(self, mb, encoding = None):
        return self.Write(mb.encode(encoding or self._encoding))

    def WriteUTF16(self, *values):
        return sum([self.Write(s.encode('U16')[2:]) for s in values])

    def WriteUTF8(self, *values):
        return sum([self.Write(s.encode('UTF8')) for s in values])

def MemoryStream(endian = FileStream.LITTLE_ENDIAN, *, data = b'', **kwargs):
    return FileStream(data, endian = endian, **kwargs)
