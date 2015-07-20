:: -------------------------------------------------------------------
:: 
:: install.bat -- Install gnuwin32 package from a local archive
::                whithin the cmd console of windows XP
:: 
:: authors: Mathias Michaelis <michaelis@tcnet.ch>
::          Jay Satiro <raysatiro@users.sourceforge.net>
:: date   : November 15, 2009
:: version: 0.6.3
::
:: Copyright (c) 2009 by Mathias Michaelis <michaelis@tcnet.ch>
:: Copyright (c) 2009 by Jay Satiro <raysatiro@users.sourceforge.net>
::
:: -------------------------------------------------------------------
::
:: Permission is granted to copy, distribute and/or modify this document
:: under the terms of the GNU Free Documentation License, Version 1.2
:: or any later version published by the Free Software Foundation;
:: with no Invariant Sections, no Front-Cover Texts, and no Back-Cover
:: Texts.  A copy of the license is included in the file LICENSE.TXT
::
::
:: Acknowledgments
:: ---------------
::
:: See file README.TXT in this directory. Thanks to all who advised me,
:: gave me some hints, sent me some patches or encouraged me to go on!
::
@echo off
setlocal ENABLEEXTENSIONS
set LC_ALL=C

set SORTPROG=sort-7.6

set GETGNUWIN32_ID=94d563d1564001

REM switch to this file's directory
cd /d "%0\.." 1>NUL 2>&1
if not exist bin\revision_%GETGNUWIN32_ID% (
	echo = 
	echo = Unique ID bin\revision_%GETGNUWIN32_ID% not found. Probable solution:
	echo = Please switch to the getgnuwin32 directory before running this script.
	echo = 
	echo = Install.bat cannot continue.
	goto bye
)

REM test ability to write to directory
set GNUWIN32_RANDFILE=%GETGNUWIN32_ID%%RANDOM%.tmp
bin\touch "%GNUWIN32_RANDFILE%"
if not exist ".\%GNUWIN32_RANDFILE%" (
	echo = 
	echo = Cannot write to directory:
	echo = "%CD%"
	echo = Please check the directory permissions.
	echo = 
	echo = Install.bat cannot continue.
	goto bye
)

REM remove temporary files
FOR %%A IN (tmp) DO (
	del "*.%%A" 1>NUL 2>&1
	if exist "*.%%A" (
		echo = 
		echo = Error: These files should not exist and could not be removed:
		dir /b "*.%%A"
		echo = 
		echo = Please make sure you have writable permissions to the
		echo = getgnuwin32 directory and all files therein.
		echo =
		echo = Install.bat cannot continue.
		goto bye
	)
)

set GNUWIN32_VERBOSE=
set GNUWIN32_HASH_MISSING=
set GNUWIN32_TESTING=
set GNUWIN32_INSTALL_DIRECTORY=gnuwin32

:command_line
if .%1==. goto command_line_end
if "%1"=="-v" goto set_verbose
if "%1"=="/v" goto set_verbose
if "%1"=="-m" goto set_hash_missing_is_ok
if "%1"=="/m" goto set_hash_missing_is_ok
if "%1"=="-j" goto set_just_testing
if "%1"=="/j" goto set_just_testing
if "%1"=="-h" goto show_help_text
if "%1"=="/h" goto show_help_text
if "%1"=="-?" goto show_help_text
if "%1"=="/?" goto show_help_text
if "%1"=="--help" goto show_help_text
if "%1"=="--version" goto show_version_text
set GNUWIN32_INSTALL_DIRECTORY=%1
shift
goto command_line

:set_verbose
set GNUWIN32_VERBOSE=TRUE
shift
goto command_line

:set_hash_missing_is_ok
set GNUWIN32_HASH_MISSING=TRUE
shift
goto command_line

:set_just_testing
REM not used yet
set GNUWIN32_TESTING=TRUE
shift
goto command_line

:show_help_text
echo = install.bat (version 0.6.3^)
echo = Copyright (c^) 2009 by Mathias Michaelis, ^<michaelis@tcnet.ch^>
echo = Copyright (c^) 2009 by Jay Satiro, ^<raysatiro@users.sourceforge.net^>
echo = This is free software; see the source for copying conditions.  There is NO
echo = warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
echo =
echo = usage:
echo =  install --version
echo =  install --help
echo =  install [-v] [-m] [target-directory]
echo =
echo = description:
echo =  install.bat, started without any command line options, will try to do
echo =  three things:
echo =  1. Unzip all verified packages in chronological order ^& check for conflicts.
echo =  2. Update the current installation (overwrite with any newer files^)
echo =  3. Separate/organize info and documentation for each package.
echo =
echo =  The behaviour of install.bat can be modified by the following options:
echo =  -v : verbose mode. There are more lines printed on screen (debug^).
echo =  -m : install unverified files missing a filename entry in packages.sha
echo =
echo = If you do not choose a directory to install to, this batch file unzips to the
echo = gnuwin32 subdirectory and you then must copy that directory to your 
echo = desired location. You can choose a directory, for example:
echo =
echo = install.bat C:\gnuwin32
echo =
echo = It is suggested that as administrator you do that or copy over to your actual
echo = target location so that appropriate security permissions are inherited.
echo = ^*Personally I suggest using a target path that does not contain spaces.
goto bye

:show_version_text
echo = install.bat (version 0.6.3^)
echo = Copyright (c^) 2009 by Mathias Michaelis, ^<michaelis@tcnet.ch^>
echo = Copyright (c^) 2009 by Jay Satiro, ^<raysatiro@users.sourceforge.net^>
echo = This is free software; see the source for copying conditions.  There is NO
echo = warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
echo =
goto bye


:command_line_end

REM check for existence of GNUWIN32_INSTALL_DIRECTORY
if not exist "%GNUWIN32_INSTALL_DIRECTORY%" mkdir "%GNUWIN32_INSTALL_DIRECTORY%"

if not exist "%GNUWIN32_INSTALL_DIRECTORY%" (
	echo =
	echo = Error: failed to create install directory:
	echo = "%GNUWIN32_INSTALL_DIRECTORY%"
	echo =
	echo = Please make sure you have writable permissions to the
	echo = target path.
	echo =
	echo = Install.bat cannot continue.
	goto bye
)

set GETGNUWIN32_DIR=%CD%

REM test ability to write to target directory
set GNUWIN32_RANDFILE=%GETGNUWIN32_ID%%RANDOM%.tmp
bin\touch "%GNUWIN32_INSTALL_DIRECTORY%\%GNUWIN32_RANDFILE%"
if not exist "%GNUWIN32_INSTALL_DIRECTORY%\%GNUWIN32_RANDFILE%" (
	echo = 
	echo = Cannot write to directory:
	echo = "%GNUWIN32_INSTALL_DIRECTORY%"
	echo = Please check the directory permissions.
	echo = 
	echo = Install.bat cannot continue.
	goto bye
)
del "%GNUWIN32_INSTALL_DIRECTORY%\%GNUWIN32_RANDFILE%" >NUL 2>&1

FOR %%A IN (bin contrib doc) DO (
	mkdir "%GNUWIN32_INSTALL_DIRECTORY%\%%A" 1>NUL 2>&1
	bin\touch "%GNUWIN32_INSTALL_DIRECTORY%\%%A\%GNUWIN32_RANDFILE%"
	if not exist "%GNUWIN32_INSTALL_DIRECTORY%\%%A\%GNUWIN32_RANDFILE%" (
		echo = 
		echo = Cannot write to directory:
		echo = "%GNUWIN32_INSTALL_DIRECTORY%\%%A"
		echo = Please check the directory permissions.
		echo = 
		echo = Install.bat cannot continue.
		goto bye
	)
	del "%GNUWIN32_INSTALL_DIRECTORY%\%%A\%GNUWIN32_RANDFILE%" >NUL 2>&1
)

set UNZIP_TOTAL_COUNT=0

bin\test -s packages\hash_pass.txt
if errorlevel 1 (
	echo =
	echo = Warning!: packages\hash_pass.txt missing or does not contain data
	dir packages\hash_pass.txt
	echo = 
	echo = Possible solutions:
	echo = Re-Run download.bat
	echo =
	echo = Continuing anyway...
) else (
	bin\sed -n "$=" packages\hash_pass.txt > count.tmp
	bin\test -s count.tmp
	if errorlevel 1 (
		echo =
		echo = Error: File missing or does not contain data: "count.tmp"
		dir "count.tmp"
		echo = 
		echo = Please make sure you have writable permissions to the
		echo = getgnuwin32 directory and all files therein.
		echo =
		echo = Download.bat cannot continue.
		goto bye
	)
	set /p UNZIP_TOTAL_COUNT=<count.tmp
	del count.tmp >NUL 2>&1
	if not defined UNZIP_TOTAL_COUNT set UNZIP_TOTAL_COUNT=0
)

echo = 
echo = Unzipping (and/or updating^) packages; also sorting documentation...
echo = 
echo = This will take some time, be patient.
echo = Interaction is necessary after the unzipping completes.
echo = 

REM Mathias' addons
bin\unzip -u -o -q bin\myaddons.zip -d "%GNUWIN32_INSTALL_DIRECTORY%" >NUL 2>&1

REM for /f won't read from a file in quotes, it will interpret it as a string
REM for has expansion issues (use delayedexpansion or call whatever_tag)
REM can't test directories with "dir\NUL" in quotes. won't work.

if defined GNUWIN32_HASH_MISSING (
	bin\test -s packages\hash_missing.txt
	if errorlevel 0 if not errorlevel 1 (
		FOR /f "tokens=*" %%f IN (packages\hash_missing.txt) DO (
			if exist "packages\%%f" (
				bin\unzip -u -o -q "packages\%%f" -d "%GNUWIN32_INSTALL_DIRECTORY%" 1>NUL 2>&1
				xcopy /Q /Y /I "%GNUWIN32_INSTALL_DIRECTORY%\contrib\*.*" "%GNUWIN32_INSTALL_DIRECTORY%\contrib\%%~nf" >NUL 2>&1
				del /F /Q "%GNUWIN32_INSTALL_DIRECTORY%\contrib\*.*" >NUL 2>&1
				xcopy /Q /Y /I "%GNUWIN32_INSTALL_DIRECTORY%\doc\*.*" "%GNUWIN32_INSTALL_DIRECTORY%\doc\%%~nf" >NUL 2>&1
				del /F /Q "%GNUWIN32_INSTALL_DIRECTORY%\doc\*.*" >NUL 2>&1
			)
		)
	)
)

REM count the total number of packages in hash_pass.txt

set UNZIP_CURRENT_COUNT=0
set PERCENT_DONE=5
set TMP_NUMBER=0
bin\test -s packages\hash_pass.txt
if errorlevel 0 if not errorlevel 1 (
	FOR /f "tokens=*" %%f IN (packages\hash_pass.txt) DO (
		if exist "packages\%%f" (
			bin\unzip -u -o -q "packages\%%f" -d "%GNUWIN32_INSTALL_DIRECTORY%" 1>NUL 2>&1
			xcopy /Q /Y /I "%GNUWIN32_INSTALL_DIRECTORY%\contrib\*.*" "%GNUWIN32_INSTALL_DIRECTORY%\contrib\%%~nf" >NUL 2>&1
			del /F /Q "%GNUWIN32_INSTALL_DIRECTORY%\contrib\*.*" >NUL 2>&1
			xcopy /Q /Y /I "%GNUWIN32_INSTALL_DIRECTORY%\doc\*.*" "%GNUWIN32_INSTALL_DIRECTORY%\doc\%%~nf" >NUL 2>&1
			del /F /Q "%GNUWIN32_INSTALL_DIRECTORY%\doc\*.*" >NUL 2>&1
		)
		call :print_percent
	)
)

echo = 
echo = Unzipping/updating and documentation sorting has completed.

cd "%GNUWIN32_INSTALL_DIRECTORY%"
if errorlevel 1 (
	echo =
	echo = Error: failed to switch to install directory:
	echo = "%GNUWIN32_INSTALL_DIRECTORY%"
	echo =
	echo = Please make sure you have writable permissions to the
	echo = directory and all files therein.
	echo =
	echo = Install.bat cannot continue.
	goto bye
)

if exist "%GETGNUWIN32_DIR%\bin\supplemental_install.bat" (
	call "%GETGNUWIN32_DIR%\bin\supplemental_install.bat" "%CD%\bin"
)

echo =
echo = Finished with packages. Fixing manifests and info entries...
echo =

::
:: COPY MANIFESTS
:: This fixes Vista problems with files it incorrectly believes require administrator access
::
copy /Y "%GETGNUWIN32_DIR%\asInvoker.manifest" bin\patch.exe.manifest >NUL 2>NUL
copy /Y "%GETGNUWIN32_DIR%\asInvoker.manifest" bin\install.exe.manifest >NUL 2>NUL
copy /Y "%GETGNUWIN32_DIR%\asInvoker.manifest" bin\install-info.exe.manifest >NUL 2>NUL
:: TESTING SHOWS EXE MODIFIED TIME AND DATE *MUST* BE LATER THAN MANIFEST
:: TOUCH INSTALL/INSTALL-INFO/PATCH AFTER MANIFEST COPY:
"%GETGNUWIN32_DIR%\bin\touch" bin\patch.exe >NUL 2>NUL
"%GETGNUWIN32_DIR%\bin\touch" bin\install.exe >NUL 2>NUL
"%GETGNUWIN32_DIR%\bin\touch" bin\install-info.exe >NUL 2>NUL

echo.

::
:: Make a list of info-entries
::
if not exist bin\install-info.exe goto orphan
if exist info\dir del info\dir
dir /a-d /b info >infolist.tmp 2>nul
if errorlevel 1 goto orphan
"%GETGNUWIN32_DIR%\bin\sed" -T "/^.*-[[:digit:]]\+\(\.gz\)\?$\|^dir$/d" infolist.tmp >infolist.txt

::
:: Update dir entries of info files
::
for /f "tokens=*" %%i in (infolist.txt) do (
	bin\install-info.exe --infodir=./info "info\%%i" 2>NUL
)

::
:: Remove orphan links, create new ones
::
:orphan
"%GETGNUWIN32_DIR%\bin\sed" -T "s/^gnuwin32\\bin\\ls -lgGRU gnuwin32 /bin\\ls -lgGRU /;s/\(\d037CD\(:[^\d037]\+\)\?\d037\)\\\{1,2\}gnuwin32/\1/g;s/gnuwin32\\\{1,2\}//g;" "%GETGNUWIN32_DIR%\update-links.bat" >update-links.bat
if not "%GNUWIN32_INSTALL_DIRECTORY%"=="gnuwin32" (
	call update-links.bat
) else (
	echo The directory "gnuwin32" can now be copied to any desired place, e.g. to the
	echo directory "C:\gnuwin32".
	echo.
	echo Note that copying is the recommended method on an NTFS file system, since the
	echo file access rights are not inherited correctly from the parent directory if
	echo the files are moved. On a FAT32 file system, there are no access rights, so
	echo there is no disadvantage to moving files.
	echo.
	echo After moving/copying the gnuwin32 folder to the desired place (pref. as
	echo administrator^) run "update-links.bat" from within that (moved/copied^)
	echo directory. That will update links to documentation.
)
echo.
echo.
echo Optional:
echo You can ^*append^* the bin subdir of your installation folder to your PATH:
echo i.e. C:\gnuwin32\bin
echo There are a few gnu conflicts with Windows utilities, so make sure to ^*append^*.
echo That way the windows utilities take priority (i.e. sort^).
echo ^*!!!!!! Please read README.txt for more information on your installation !!!!!^*
echo.

echo Done installing. yay. Have fun!
goto bye

:print_percent
set /a UNZIP_CURRENT_COUNT+=1
REM echo percentdone%PERCENT_DONE% total count%UNZIP_TOTAL_COUNT%
set /a TMP_NUMBER="(UNZIP_TOTAL_COUNT/20)*(PERCENT_DONE/5)"
REM echo %UNZIP_CURRENT_COUNT% %TMP_NUMBER%
if /i %UNZIP_CURRENT_COUNT% GTR %TMP_NUMBER% (
	echo = %PERCENT_DONE% percent done unzipping / organizing...
	set /a PERCENT_DONE+=5
)
goto :EOF

:bye
if exist filelist.tmp del filelist.tmp >NUL 2>&1
if exist infolist.txt del infolist.txt >NUL 2>&1
if exist infolist.tmp del infolist.tmp >NUL 2>&1

endlocal
pause
