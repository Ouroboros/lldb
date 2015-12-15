@echo off
cd/d "%~dp0"

set "PYTHON_HOME=%~dp0..\Python35\x86\Lib\site-packages\lldb"
del "%PYTHON_HOME%\_lldb.pyd"
del "%PYTHON_HOME%\lldb-argdumper.exe"
del .lldbinit
del cmds.py

mklink /H "%PYTHON_HOME%\_lldb.pyd" "%~dp0liblldb.dll"
mklink /H "%PYTHON_HOME%\lldb-argdumper.exe" "%~dp0lldb-argdumper.exe"
mklink /H .lldbinit "..\bin\.lldbinit"
mklink /H cmds.py "..\bin\cmds.py"

start lldb.exe
