@echo off

start /B "" "%~dp0tcprelay.py" -t 22:2222
start /B "" "%~dp0tcprelay.py" -t 5000:5000
call "%~dp0tcprelay.py" -t 8888:4321
