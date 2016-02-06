import rsa
import base64
from pyasn1.type.error import PyAsn1Error

class RsaCipher:
    def __init__(self, key, *, Fast = False):
        if Fast:
            from Crypto.PublicKey import RSA
            from Crypto.Random import random

            try:
                self.key = RSA.importKey(key)
            except ValueError:
                raise

            self.random = random.StrongRandom()

            if self.key.has_private():
                self.encrypt_block = self.fast_private_encrypt_block
                self.decrypt_block = self.fast_private_decrypt_block
            else:
                self.encrypt_block = self.fast_public_encrypt_block
                self.decrypt_block = self.fast_public_decrypt_block

            self.DecryptBlockSize = (self.key.size() + 1) // 8
            self.EncryptBlockSize = self.DecryptBlockSize - 11

        else:
            try:
                self.key = rsa.key.PublicKey.load_pkcs1(key, 'DER')
                self.encrypt_block = self.slow_public_encrypt_block
                self.decrypt_block = self.slow_public_decrypt_block

            except PyAsn1Error:
                self.key = rsa.key.PrivateKey.load_pkcs1(key, 'DER')
                self.encrypt_block = self.slow_private_encrypt_block
                self.decrypt_block = self.slow_private_decrypt_block

            self.DecryptBlockSize = rsa.common.byte_size(self.key.n)
            self.EncryptBlockSize = self.DecryptBlockSize - 11

    def encrypt(self, message, *, encoding = 'UTF8'):
        if isinstance(message, str):
            message = message.encode('UTF8')

        crypto = bytearray()
        for start in range(0, len(message), self.EncryptBlockSize):
            crypto.extend(self.encrypt_block(message[start : start + self.EncryptBlockSize], self.key))

        return crypto

    def decrypt(self, crypto, *, encoding = None):
        message = bytearray()
        for start in range(0, len(crypto), self.DecryptBlockSize):
            message.extend(self.decrypt_block(crypto[start : start + self.DecryptBlockSize], self.key))

        return encoding is None and message or message.decode(encoding)

    def encryptstring(self, string, *, encoding = 'UTF8'):
        return base64.encodebytes(self.encrypt(string.encode(encoding))).decode('ASCII')

    def decryptstring(self, crypto, *, encoding = 'UTF8'):
        return self.decrypt(base64.decodebytes(crypto.encode('ASCII'))).decode(encoding)

    def slow_private_decrypt_block(self, crypto, key):
        return rsa.decrypt(crypto, key)

    def slow_private_encrypt_block(self, message, key):
        pub = rsa.key.PublicKey(n = key.n, e = key.d)
        return rsa.encrypt(message, pub)

    def slow_public_encrypt_block(self, message, key):
        return rsa.encrypt(message, key)

    def slow_public_decrypt_block(self, crypto, key):
        private = rsa.key.PrivateKey(n = key.n, e = 0, d = key.e, p = 0, q = 0, exp1 = 0, exp2 = 0, coef = 0)
        return rsa.decrypt(crypto, private)

    def fast_private_decrypt_block(self, crypto, key):
        message = key.decrypt(crypto)
        print(message)
        return message[message.index(b'\x00', 2) + 1:]

    def fast_private_encrypt_block(self, message, key):
        # N, E = key.n, key.d
        message = rsa.pkcs1._pad_for_encryption(message, key.size() + 1)
        return key.implementation.construct((key.n, key.d)).encrypt(message, self.random.randrange(999))[0]

    def fast_public_encrypt_block(self, message, key):
        message = rsa.pkcs1._pad_for_encryption(message, key.size() + 1)
        return key.encrypt(message, self.random.randrange(999))[0]

    def fast_public_decrypt_block(self, crypto, key):
        # N, D = key.n, key.e
        pk = key.implementation.construct((key.n, 0))
        pk.d = key.e
        message = pk.decrypt(crypto)
        return message[message.index(b'\x00', 2) + 1:]
