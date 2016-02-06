import os
from ..fileio import getDirectoryFiles

def forEachFile(callback, pathlist, filter = '*.*'):
    if not isinstance(pathlist, (list, tuple)):
        pathlist = [pathlist]

    files = []
    for p in pathlist:
        files.extend(getDirectoryFiles(p, filter)) if os.path.isdir(p) else files.append(p)

    return list(map(callback, files))

def forEachFileMP(callback, pathlist, filter = '*.*'):
    import multiprocessing
    from multiprocessing.pool import Pool

    if not isinstance(pathlist, (list, tuple)):
        pathlist = [pathlist]

    files = []
    for p in pathlist:
        files.extend(getDirectoryFiles(p, filter)) if os.path.isdir(p) else files.append(p)

    with Pool(multiprocessing.cpu_count()) as p:
        return p.map(callback, files)
