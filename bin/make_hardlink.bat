::@echo off

set "PYTHON_HOME=%~dp0..\Python35\x86\Lib\site-packages\lldb"
del "%PYTHON_HOME%\_lldb.pyd"
mklink /H "%PYTHON_HOME%\_lldb.pyd" "%~dp0liblldb.dll"
pause
