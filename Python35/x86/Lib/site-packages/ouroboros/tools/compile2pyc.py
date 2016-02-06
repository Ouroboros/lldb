from ml import *
import py_compile, shutil, zipfile, uuid

def get_random_filename():
    name = uuid.uuid4().hex
    return name[:1]

def main():
    PYC_HEADER_SIZE = 12

    for path in sys.argv[1:]:
        src = os.path.abspath(path)
        dst = src + '.Compiled'

        for f in fileio.getDirectoryFiles(path):
            if '__pycache__' in f:
                continue

            print(f)

            o = f.replace(src, dst, 1)
            os.makedirs(os.path.dirname(o), exist_ok = True)

            if f.lower().endswith('.py'):
                pyo = os.path.splitext(o)[0] + '.pyc'
                ret = py_compile.compile(f, pyo, optimize = 2)
                if ret is None:
                    raise Exception('error occured while compiling %s -> %s' % (f, pyo))
                with open(pyo, 'rb+') as fs:
                    # fs.seek(PYC_HEADER_SIZE)
                    buf = fs.read()
                    # if not pyo.endswith('ppinit'):
                    #     buf = private.encrypt(buf)

                    fs.seek(0)
                    fs.write(buf)
            else:
                try:
                    samefile = os.path.samefile(f, o)
                except FileNotFoundError:
                    samefile = False

                if not samefile:
                    try:
                        shutil.copy2(f, o)
                    except PermissionError:
                        pass

    # PauseConsole()

if __name__ == '__main__':
    TryInvoke(main)
