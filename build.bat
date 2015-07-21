@echo off
cd/d "%~dp0"

set PATH=%~dp0tools\GetGnuWin32\bin;%~dp0tools\swigwin-3.0.5;%~dp0tools\cmake-3.2.2-win32-x86\bin;%PATH%
set LLVM=%~dp0llvm\llvm
set LLDB=%~dp0llvm\lldb
set CLANG=%~dp0llvm\clang

rd "%LLVM%\tools\lldb" >NUL 2>NUL
rd "%LLVM%\tools\clang" >NUL 2>NUL

mklink /d "%LLVM%\tools\lldb" "%LLDB%"
mklink /d "%LLVM%\tools\clang" "%CLANG%"

IF NOT [%ERRORLEVEL%] == [0] echo mklink failed && pause

call "%VS120COMNTOOLS%..\..\VC\vcvarsall.bat" x86

set INCLUDE=%INCLUDE%;%~dp0external

mkdir build
cd build

cmake -G Ninja "%~dp0llvm\llvm" -DCMAKE_BUILD_TYPE=RelWithDebInfo

echo.
echo before you run "ninja lldb", you may replace all "/INCREMENTAL" in build.ninja with "/OPT:REF"
echo to reduce the binary size
echo.

cmd/k

rd "%LLVM%\tools\lldb" >NUL 2>NUL
rd "%LLVM%\tools\clang" >NUL 2>NUL

pause
