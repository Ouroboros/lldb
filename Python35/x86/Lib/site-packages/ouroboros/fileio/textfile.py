def readLines(filename, cp = '936'):
    buf = open(filename,'rb').read()

    if buf[0:2] == b'\xff\xfe':
        buf = buf.decode('U16')
    elif buf[0:3] == b'\xef\xbb\xbf':
        buf = buf.decode('utf-8-sig')
    else:
        try:
            buf = buf.decode('UTF8')
        except UnicodeDecodeError:
            buf = buf.decode(cp)

    return buf.splitlines()
