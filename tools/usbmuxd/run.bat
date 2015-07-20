@echo off

set python27=D:\Dev\Python27\python.exe

start /B "" "%python27%" "%~dp0tcprelay.py" -t 22:2222
call "%python27%" "%~dp0tcprelay.py" -t 8888:4321
