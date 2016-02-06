class dict2(dict):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        if kwargs.get('deepConvert') is not False:
            self.convertRecursive(self)

    def convertRecursive(self, obj):
        for k, v in obj.items():
            if not isinstance(v, dict):
                continue

            v = dict2(v)
            obj[k] = v
            self.convertRecursive(v)

    def __setattr__(self, name, value):
        return super().__setitem__(name, value)

    def __getattr__(self, name):
        try:
            return super().__getattr__(name)
        except AttributeError:
            pass

        try:
            return super().__getitem__(name)
        except KeyError:
            raise AttributeError(''''%s' object has no attribute '%s' ''' % (type(self), name))

    def __deepcopy__(self, memo):
        import copy
        return copy._deepcopy_dict(self, memo)