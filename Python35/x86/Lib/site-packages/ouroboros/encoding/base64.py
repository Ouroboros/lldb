def base64Encode(data, *, encoding = 'UTF8', multiline = False):
    import base64

    if isinstance(data, str):
        data = data.encode(encoding)

    data = base64.encodebytes(data)

    if not multiline:
        data = data.replace(b'\r', b'').replace(b'\n', b'')

    return data.decode('ASCII')

def base64Decode(data, *, encoding = None):
    import base64

    if isinstance(data, str):
        data = data.encode('ASCII')

    data = base64.decodebytes(data)

    if encoding:
        data = data.decode(encoding)

    return data
