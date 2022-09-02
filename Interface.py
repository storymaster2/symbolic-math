def abstractmethod(func):
    func.__isabstract__ = True
    return func

class Interface(type):
    def __init__(self, name, bases, namespace):
        for base in bases:
            mustImplement = getattr(base, '__abstractMethods', [])
            classMethods = getattr(self, '__allMethods', [])
            for method in mustImplement:
                if method not in classMethods:
                    errStr = """Can't create abstract class {name}! {name} must implement abstract method {method} of class {base_class}!""".format(name=name, method=method, base_class=base.__name__)
                    raise TypeError(errStr)

    def __new__(metaclass, name, bases, namespace):
        namespace['__abstractMethods'] = Interface.__getAbstractMethods(namespace)
        namespace['__allMethods'] = Interface.__getAllMethods(namespace)
        return super().__new__(metaclass, name, bases, namespace)
    
    def __getAbstractMethods(namespace):
        return [name for name, val in namespace.items() if callable(val) and getattr(val, '__isabstract__', False)]

    def __getAllMethods(namespace):
        return [name for name, val in namespace.items() if callable(val)]