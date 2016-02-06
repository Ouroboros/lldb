import os, glob, fnmatch

def getDirectoryFiles(path, filter = '*.*'):
    allfiles = []
    if filter == '*.*':
        filter = '*'
    elif not filter.startswith('*'):
        filter = '*' + filter

    for root, dirs, files in os.walk(path):
        for f in files:
            f = os.path.join(root, f)
            fnmatch.filter([f], filter) and not os.path.isdir(f) and allfiles.append(f)

    return allfiles
