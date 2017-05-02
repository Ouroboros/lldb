@echo off
cd/d "%~dp0"

set "PYTHON_HOME=%~dp0..\Python36\amd64\Lib\site-packages\lldb"
del "%PYTHON_HOME%\_lldb.pyd" 2>NUL
del "%PYTHON_HOME%\lldb-argdumper.exe" 2>NUL
del "%~dp0python36.dll"
mklink /H "%~dp0python36.dll" "%PYTHON_HOME%\..\..\..\python36.dll"
mklink /H "%PYTHON_HOME%\_lldb.pyd" "%~dp0liblldb.dll"
mklink /H "%PYTHON_HOME%\lldb-argdumper.exe" "%~dp0lldb-argdumper.exe"

start lldb.exe
