class HashResult:
    def __init__(self, digest):
        self.hex = digest
        self.bytes = digest.hex()

    def __bytes__(self):
        return self._bytes

    def __str__(self):
        return self._hex

    __repr__ = __str__

def hashBytes(bytes, method = 'md5'):
    import hashlib

    h = getattr(hashlib, method.lower())()
    h.update(bytes)
    return h.digest()

def md5(data):
    return hashBytes(data, 'md5')

def sha1(data):
    return hashBytes(data, 'sha1')

def sha256(data):
    return hashBytes(data, 'sha256')

def sha512(data):
    return hashBytes(data, 'sha512')
