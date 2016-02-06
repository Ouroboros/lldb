import collections

class CaseInsensitiveDict(dict):
    """Dictionary that enables case insensitive searching while preserving case sensitivity
when keys are listed, ie, via keys() or items() methods.

Works by storing a lowercase version of the key as the new key and stores the original key-value
pair as the key's value (values become dictionaries)."""

    def __init__(self, initval = None):

        if isinstance(initval, dict):
            for key, value in initval.items():
                self.__setitem__(key, value)

        elif isinstance(initval, collections.Iterable):
            for (key, value) in initval:
                self.__setitem__(key, value)

    def __contains__(self, key):
        return dict.__contains__(self, key.lower())

    def __getitem__(self, key):
        return dict.__getitem__(self, key.lower())['val']

    def __setitem__(self, key, value):
        return dict.__setitem__(self, key.lower(), {'key': key, 'val': value})

    def __delitem__(self, key):
        return super().__delitem__(key.lower())

    def setdefault(self, key, default):
        if key not in self:
            self[key] = default

    def get(self, key, default = None):
        try:
            v = dict.__getitem__(self, key.lower())
        except KeyError:
            return default
        else:
            return v['val']

    def pop(self, key):
        kv = dict.pop(self, key.lower())
        return kv['val']

    def items(self):
        return [(v['key'], v['val']) for v in dict.values(self)]

    def keys(self):
        return [v['key'] for v in dict.values(self)]

    def values(self):
        return [v['val'] for v in dict.values(self)]
