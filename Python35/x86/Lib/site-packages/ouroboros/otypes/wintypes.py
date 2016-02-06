import ctypes
# from ctypes.wintypes import *

BYTE = ctypes.c_byte
WORD = ctypes.c_ushort
DWORD = ctypes.c_ulong

ULONG = ctypes.c_ulong
LONG = ctypes.c_long

USHORT = ctypes.c_ushort
SHORT = ctypes.c_short

LPCOLESTR = LPOLESTR = OLESTR = ctypes.c_wchar_p
LPCWSTR = LPWSTR = ctypes.c_wchar_p
LPCSTR = LPSTR = ctypes.c_char_p
LPCVOID = LPVOID = ctypes.c_void_p

CHAR = ctypes.c_char
WCHAR = ctypes.c_wchar
UINT = ctypes.c_uint
INT = ctypes.c_int

DOUBLE = ctypes.c_double
FLOAT = ctypes.c_float

BOOLEAN = BYTE
BOOL = ctypes.c_long

CHAR        = ctypes.c_char
BYTE        = ctypes.c_ubyte      # fix bug: BYTE == CHAR
UCHAR       = BYTE

PLONG       = ctypes.POINTER(LONG)
PULONG      = ctypes.POINTER(ULONG)

INT64       = ctypes.c_int64
UINT64      = ctypes.c_uint64

LONG64      = ctypes.c_longlong
LONGLONG    = LONG64
ULONG64     = ctypes.c_ulonglong
ULONGLONG   = ULONG64

PLONG64     = ctypes.POINTER(LONG64)
PULONG64    = ctypes.POINTER(ULONG64)

PVOID       = ctypes.c_void_p
PSTR        = LPSTR
PWSTR       = LPWSTR

NTSTATUS    = LONG


if ctypes.sizeof(PVOID) == ctypes.sizeof(ULONG):
    INT_PTR     = INT
    UINT_PTR    = UINT
    LONG_PTR    = LONG
    ULONG_PTR   = ULONG

elif ctypes.sizeof(PVOID) == ctypes.sizeof(ULONG64):
    INT_PTR     = INT64
    UINT_PTR    = UINT64
    LONG_PTR    = LONG64
    ULONG_PTR   = ULONG64

PINT_PTR        = ctypes.POINTER(INT_PTR)
PUINT_PTR       = ctypes.POINTER(UINT_PTR)
PLONG_PTR       = ctypes.POINTER(LONG_PTR)
PULONG_PTR      = ctypes.POINTER(ULONG_PTR)