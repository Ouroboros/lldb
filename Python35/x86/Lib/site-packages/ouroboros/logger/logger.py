import logging
import os, io, sys
import datetime
import types
import traceback
import hexdump

__FORMAT__ = '[%(asctime)s][%(thread)d][%(filename)s][%(funcName)s:%(lineno)d][%(levelname)s] %(message)s'

logging.basicConfig(
    level       = 999,
    format      = __FORMAT__,
    datefmt     = '%Y-%m-%d %H:%M:%S'
)

def getLogger(name, *, logPath = None):
    def init(self, *, level = logging.DEBUG, handlerClass = logging.FileHandler):
        logFormatter = logging.Formatter(__FORMAT__)
        fileHandler = handlerClass(filename = self.getLogFileName(), mode = 'w', encoding = 'UTF_8_SIG')
        fileHandler.setFormatter(logFormatter)

        if handlerClass is not logging.FileHandler:
            self.root.handlers.clear()

        self.addHandler(fileHandler)
        self.setLevel(level)

        self.funcNameFilter = ['log', 'info', 'warn', 'debug', 'error', 'logException', 'logHexdump']

    def attach(self, obj):
        for func in self.funcNameFilter:
            setattr(obj, func, getattr(self, func))

    def getLogFileName(self):
        import threading

        path = logPath or os.path.join(os.path.abspath(os.path.dirname(sys.argv[0])), 'logs')
        os.makedirs(path, exist_ok = True)
        t = datetime.datetime.now()
        return os.path.join(path, '[%s][%s][%04d-%02d-%02d %02d.%02d.%02d][%d][%d].txt' % (os.path.basename(sys.argv[0]).rsplit('.', maxsplit = 1)[0], self.name, t.year, t.month, t.day, t.hour, t.minute, t.second, os.getpid(), threading.currentThread().ident))

    def findCaller(self, stack_info = False):
        """
        Find the stack frame of the caller so that we can note the source
        file name, line number and function name.
        """
        f = logging.currentframe()
        #On some versions of IronPython, currentframe() returns None if
        #IronPython isn't run with -X:Frames.
        if f is not None:
            f = f.f_back
        rv = "(unknown file)", 0, "(unknown function)", None
        while hasattr(f, "f_code"):
            co = f.f_code
            filename = os.path.normcase(co.co_filename)
            if filename == logging._srcfile or co.co_name in self.funcNameFilter:
                f = f.f_back
                continue
            sinfo = None
            if stack_info:
                sio = io.StringIO()
                sio.write('Stack (most recent call last):\n')
                traceback.print_stack(f, file=sio)
                sinfo = sio.getvalue()
                if sinfo[-1] == '\n':
                    sinfo = sinfo[:-1]
                sio.close()
            rv = (co.co_filename, f.f_lineno, co.co_name, sinfo)
            break
        return rv

    def logException(self):
        self.debug('\n' + '\r\n'.join(traceback.format_exception(*sys.exc_info())))

    def logHexdump(self, data, msg = ''):
        self.debug(('%s\n\n' % msg) + hexdump.hexdump(data, result = 'return') + '\n')

    logger = logging.getLogger(name)
    logger.init           = types.MethodType(init, logger)
    logger.attach         = types.MethodType(attach, logger)
    logger.findCaller     = types.MethodType(findCaller, logger)
    logger.getLogFileName = types.MethodType(getLogFileName, logger)
    logger.logException   = types.MethodType(logException, logger)
    logger.logHexdump     = types.MethodType(logHexdump, logger)

    return logger
