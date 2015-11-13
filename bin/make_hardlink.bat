@echo off

set "PYTHON_HOME=%~dp0..\Python35\amd64\Lib\site-packages\lldb"
del "%PYTHON_HOME%\_lldb.pyd"
del "%PYTHON_HOME%\lldb-argdumper.exe"
mklink /H "%PYTHON_HOME%\_lldb.pyd" "%~dp0liblldb.dll"
mklink /H "%PYTHON_HOME%\lldb-argdumper.exe" "%~dp0lldb-argdumper.exe"
pause
