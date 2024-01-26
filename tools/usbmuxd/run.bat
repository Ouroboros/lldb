@echo off
call "%~dp0tcprelay.py" -t 8888:4321 22:2222 39001:39001
