def encodeVarint(value):
    varint = b''

    bits = value & 0x7f
    value >>= 7

    while value:
        varint += int.to_bytes(0x80 | bits, 1, 'little')
        bits = value & 0x7f
        value >>= 7

    varint += int.to_bytes(bits, 1, 'little')

    return varint

def decodeVarint(buffer, mask = (1 << 64) - 1):
    pos    = 0
    result = 0
    shift  = 0

    while True:
        b = buffer[pos]
        result |= ((b & 0x7f) << shift)
        pos += 1
        if not (b & 0x80):
            buffer
            result &= mask
            return result, pos

        shift += 7
        if shift >= 64:
            raise Exception('Too many bytes when decoding varint.')
