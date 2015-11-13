'''
@echo off
cd/d "%~dp0"
cls

set PATH=%~dp0tools\GetGnuWin32\bin;%~dp0tools\swigwin-3.0.5;%~dp0tools\cmake-3.2.2-win32-x86\bin;%PATH%
set LLVM=%~dp0llvm\llvm
set LLDB=%~dp0llvm\lldb
set CLANG=%~dp0llvm\clang
set ARCH=amd64
set BUILD=build_%ARCH%

rd "%LLVM%\tools\lldb" >NUL 2>NUL
rd "%LLVM%\tools\clang" >NUL 2>NUL

mklink /j "%LLVM%\tools\lldb" "%LLDB%"
mklink /j "%LLVM%\tools\clang" "%CLANG%"

IF NOT [%ERRORLEVEL%] == [0] (
    echo mklink failed
    pause
)

call "%VS140COMNTOOLS%..\..\VC\vcvarsall.bat" %ARCH%

set INCLUDE=%INCLUDE%;%~dp0external

mkdir %BUILD%
cd %BUILD%

if not exist build.ninja (
    cmake -G Ninja "%~dp0llvm\llvm" -DCMAKE_BUILD_TYPE=RelWithDebInfo -DPYTHON_HOME=%~dp0Python35\%ARCH%\ -DPYTHON_EXECUTABLE=%~dp0Python35\%ARCH%\python.exe
    call py "%~f0" "%~dp0%BUILD%\build.ninja"
)

echo.
echo before run "ninja lldb", you may replace all "/INCREMENTAL" in build.ninja with "/OPT:REF"
echo to reduce the binary size
echo.

cmd/k

rd "%LLVM%\tools\lldb" >NUL 2>NUL
rd "%LLVM%\tools\clang" >NUL 2>NUL

goto:eof

'''

if __name__ == '__main__':
    import os, sys
    b = open(sys.argv[1], 'rb').read().replace(b'/INCREMENTAL', b'/OPT:REF')
    open(sys.argv[1], 'wb').write(b)
