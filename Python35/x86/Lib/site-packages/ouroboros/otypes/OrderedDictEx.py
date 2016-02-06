import collections

class OrderedDictEx(collections.OrderedDict):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        if kwargs.get('deepConvert') is not False:
            self.convertRecursive(self)

    def convertRecursive(self, obj):
        for k, v in obj.items():
            if not isinstance(v, dict):
                continue

            v = type(self)(v)
            obj[k] = v
            self.convertRecursive(v)

    def __getattr__(self, name):
        try:
            return super().__getattr__(name)
        except AttributeError:
            pass

        try:
            return super().__getitem__(name)
        except KeyError:
            pass

        raise AttributeError(''''%s' object has no attribute '%s' ''' % (type(self), name))
