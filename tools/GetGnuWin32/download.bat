:: -------------------------------------------------------------------
:: 
:: download.bat -- Maintain a small local gnuwin32 package archive
::                 within the cmd console of windows XP
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

REM GETGNUWIN32_ID is unique to this revision of download.bat
REM GETGNUWIN32_ID/SITE must not contain spaces (important). this includes trailing spaces!
set GETGNUWIN32_ID=94d563d1564001
set GETGNUWIN32_SITE=getgnuwin32.sourceforge.net
set GETGNUWIN32_ARGV=%*

REM switch to this file's directory
cd /d "%0\.." 1>NUL 2>&1
if not exist bin\revision_%GETGNUWIN32_ID% (
	echo = 
	echo = Unique ID bin\revision_%GETGNUWIN32_ID% not found. Probable solution:
	echo = Please switch to the getgnuwin32 directory before running this script.
	echo = 
	echo = Download.bat cannot continue.
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
	echo = Download.bat cannot continue.
	goto bye
)

REM Find Wget revision to use

set WGETPROG=placeholder123
if exist bin\wgetname.dat (
set /p WGETPROG=<bin\wgetname.dat
) else (
	echo = 
	echo = Error: Cannot find bin\wgetname.dat
	echo = "%CD%"
	echo = 
	echo = Download.bat cannot continue.
	goto bye
)
if not defined WGETPROG set WGETPROG=placeholder123

if "%WGETPROG%" == "placeholder123" (
	echo = 
	echo = Error: Cannot find name of wget executable in bin\wgetname.dat
	echo = "%CD%"
	echo = 
	echo = Download.bat cannot continue.
	goto bye
)

if not exist "bin\%WGETPROG%" (
	echo = 
	echo = Error: Cannot find "bin\%WGETPROG%" 
	echo = "%CD%"
	echo = 
	echo = Download.bat cannot continue.
	goto bye
)

REM remove temporary files
FOR %%A IN (out tmp) DO (
	del "*.%%A" 1>NUL 2>&1
	if exist "*.%%A" (
		echo = 
		echo = Error: These files should not exist and could not be removed:
		dir /b "*.%%A"
		echo = 
		echo = Please make sure you have writable permissions to the
		echo = getgnuwin32 directory and all files therein.
		echo =
		echo = Download.bat cannot continue.
		goto bye
	)
)
::
:: Customization
:: -------------
::
:: Select one or more of the following mirrors first! Please separate
:: the mirrors by space, e.g. GNUWIN32_MIRROR="iweb voxel".
::
REM 
REM
REM http://sourceforge.net/apps/trac/sourceforge/wiki/Mirrors
REM copy mirror table, paste in notepad++ then
REM Search regexp: ([a-zA-Z-]*)[ \t]([^ \t].*)
REM Replace regexp: REM \1\t[ \2 ]
REM
REM November 5, 2009:
REM
REM biznetnetworks	[ Indonesia (Asia) ]
REM dfn			[ Berlin, Germany (Europe) ]
REM fastbull	[ Italy (Europe) ]
REM freefr		[ France (Europe) ]
REM garr		[ Bologna, Italy (Europe) ]
REM heanet		[ Dublin, Ireland (Europe) ]
REM hivelocity	[ Tampa, FL, USA (North America) ]
REM internap	[ San Jose, CA, USA (North America) ]
REM internode	[ Adelaide, Australia (Australia) ]
REM iweb		[ Quebec, Canada (North America) ]
REM jaist		[ Ishikawa, Japan (Asia) ]
REM kent		[ Kent, UK (Europe) ]
REM mesh		[ Duesseldorf, Germany (Europe) ]
REM nchc		[ Tainan, Taiwan (Asia) ]
REM ncu			[ Taiwan (Asia) ]
REM nfsi		[ Linda-a-Velha, Portugal (Europe) ]
REM ovh			[ Paris, France (Europe) ]
REM puzzle		[ Bern, Switzerland (Europe) ]
REM softlayer	[ Plano, TX , USA (North America) ]
REM sunet		[ Sweden (Europe) ]
REM superb-east	[ McLean, VA, USA (North America)  ]
REM superb-west	[ Seattle, WA, USA (North America) ]
REM surfnet		[ Amsterdam, The Netherlands (Europe) ]
REM switch		[ Lausanne, Switzerland (Europe) ]
REM transact	[ Canberra, Australia (Australia) ]
REM ufpr		[ Curitiba, Brazil (South America) ]
REM voxel		[ New York, NY, USA (North America) ]
REM waix		[ Perth, Australia (Australia) ]

if .%GNUWIN32_MIRROR%==. set GNUWIN32_MIRROR=iweb voxel

::
:: Process command line options.
::
set GNUWIN32_CMDLINE_MIRROR=
set GNUWIN32_VERBOSE=
set GNUWIN32_UPDATE_ONLY=
set GNUWIN32_DIRECT_DOWNLOAD=
set GNUWIN32_IGNORE_ERR=
set GNUWIN32_TESTING=

:command_line
if .%1==. goto command_line_end
if "%1"=="-v" goto set_verbose
if "%1"=="/v" goto set_verbose
if "%1"=="-u" goto set_update_only
if "%1"=="/u" goto set_update_only
if "%1"=="-d" goto set_direct_download
if "%1"=="/d" goto set_direct_download
if "%1"=="-c" goto set_continue_on_error
if "%1"=="/c" goto set_continue_on_error
if "%1"=="-j" goto set_just_testing
if "%1"=="/j" goto set_just_testing
if "%1"=="-h" goto show_help_text
if "%1"=="/h" goto show_help_text
if "%1"=="-?" goto show_help_text
if "%1"=="/?" goto show_help_text
if "%1"=="--help" goto show_help_text
if "%1"=="--version" goto show_version_text
set GNUWIN32_CMDLINE_MIRROR=%GNUWIN32_CMDLINE_MIRROR% %1
shift
goto command_line

:set_verbose
set GNUWIN32_VERBOSE=TRUE
shift
goto command_line

:set_update_only
set GNUWIN32_UPDATE_ONLY=TRUE
shift
goto command_line

:set_direct_download
set GNUWIN32_DIRECT_DOWNLOAD=TRUE
shift
goto command_line

:set_continue_on_error
set GNUWIN32_IGNORE_ERR=TRUE
shift
goto command_line

:set_just_testing
REM not used yet
set GNUWIN32_TESTING=TRUE
shift
goto command_line

:show_help_text
echo = download.bat (version 0.6.3^)
echo = Copyright (c^) 2009 by Mathias Michaelis, ^<michaelis@tcnet.ch^>
echo = Copyright (c^) 2009 by Jay Satiro, ^<raysatiro@users.sourceforge.net^>
echo = This is free software; see the source for copying conditions.  There is NO
echo = warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
echo =
echo = usage:
echo =  download --version
echo =  download --help
echo =  download [-v] [-d] [-u] [-c] [mirror] [mirror] [mirror] [mirror]
echo =
echo = description:
echo =  download.bat, started without any command line options, will try to do
echo =  two things:
echo =  1. Check and install a signed update of GetGnuWin32 containing a list
echo =     of all gnuwin32 project files (bin/dep/doc/lib.lst^) and hashes.
echo =  2. Download the latest releases in bin/dep/doc/lib.lst, except the files
echo =     listed in include.txt, for which exactly therein specified versions are 
echo =     downloaded, and except the files listed in exclude.txt.
echo =
echo =  The behaviour of download.bat can be modified by the following options:
echo =  -v : verbose mode. There are more lines printed on screen (debug^).
echo =  -d : direct download. Do not update; step 1 is skipped.
echo =  -u : update only. GetGnuWin32 attempts to update only; step 2 is skipped.
echo =  -c : continue on (some^) errors.
echo =
echo = download.bat is set to try the following sourceforge mirrors:
echo = %GNUWIN32_MIRROR%
echo = reference: http://sourceforge.net/apps/trac/sourceforge/wiki/Mirrors
echo = if you like you can specify your own sourceforge mirror(s^) preference:
echo = i.e. download.bat switch puzzle
goto bye

:show_version_text
echo = download.bat (version 0.6.3^)
echo = Copyright (c^) 2009 by Mathias Michaelis, ^<michaelis@tcnet.ch^>
echo = Copyright (c^) 2009 by Jay Satiro, ^<raysatiro@users.sourceforge.net^>
echo = This is free software; see the source for copying conditions.  There is NO
echo = warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
echo =
goto bye

::
:: Verify if command lines are meaningful
::
:command_line_end
if defined GNUWIN32_UPDATE_ONLY (
	if defined GNUWIN32_DIRECT_DOWNLOAD (
		echo = Error: -u and -d option are specified together. There is nothing to do!
		echo = 
		echo = Download.bat cannot continue.
		goto bye
	)
)

::
:: If command line options were specified, overwrite environment
:: variables
::
if "%GNUWIN32_CMDLINE_MIRROR%"=="" goto environment
set GNUWIN32_MIRROR=%GNUWIN32_CMDLINE_MIRROR:~1%
set GNUWIN32_LOAD=
set GNUWIN32_CMDLINE_MIRROR=

::
:: After the mirrors are known, the download address of those mirrors
:: can be constructed (if not already done):
::
:environment
if not .%GNUWIN32_LOAD%==. goto set_wgetrc
set GNUWIN32_LOAD=http://%GNUWIN32_MIRROR:"=%.dl.sourceforge.net/project/gnuwin32
set GNUWIN32_LOAD=%GNUWIN32_LOAD: =.dl.sourceforge.net/project/gnuwin32 http://%

::
:: Perhaps the user has specified her/his own .wgetrc file by means
:: of the WGETRC environment variable. If not, then use the one in
:: the local bin directory
::
:set_wgetrc
if ".%WGETRC%"=="." set WGETRC=%CD%\bin\wget.ini

::
:: If we are in verbose mode, tell what we're going to do
::
if not defined GNUWIN32_VERBOSE goto test_connection

echo =
echo = List of mirrors (high priority first^):
echo =  %GNUWIN32_MIRROR%
echo =
echo = List of URLs (high priority first^):
echo %GNUWIN32_LOAD% >loadlist.txt
bin\sed -T "s/  *$//;s/.*/=  &/;s/[^ =] /\n=  /g" loadlist.txt
echo =
del loadlist.txt


:test_connection
if defined GNUWIN32_VERBOSE echo = testing %GETGNUWIN32_SITE% connection
bin\%WGETPROG% --max-redirect=0 --spider http://%GETGNUWIN32_SITE% 1>sf_diag.tmp 2>&1
bin\sed -n "/Connecting to.*connected\./p" sf_diag.tmp > sf_connect.tmp
REM "Connecting to" is english text; we have no gettext support in wget 1.12.1-devel so this is ok for now.
bin\test -s sf_connect.tmp
if errorlevel 1 (
	echo = 
	echo = Error: %GETGNUWIN32_SITE% test connection failed.
REM	if defined GNUWIN32_VERBOSE (
		echo =
		echo = here's wget output:
		type sf_diag.tmp 2>NUL
REM	)
	echo =
	echo = Possible solutions:
	echo = Do you need a proxy for internet access^? If so, set one in bin\wget.ini
	echo = The server may be experiencing downtime. Try again later.
	if not defined GNUWIN32_IGNORE_ERR (
		echo =
		echo = To continue on error you must specify the -c option.
		echo =
		echo = Download.bat cannot continue.
		del sf_connect.tmp 1>NUL 2>&1
		goto bye
	)
	echo =
	echo = Continuing anyway...
)
del sf_connect.tmp 1>NUL 2>&1

if defined GNUWIN32_DIRECT_DOWNLOAD (
REM the user just wants to download without checking for updates.
	if defined GNUWIN32_VERBOSE (
		echo = 
		echo = Command line option -d specified, update check skipped.
	)
	set UPDATE_FAILED=
	goto update_check_finished
)

REM must not have spaces in UPDATE_NAME. same is noted for GETGNUWIN32_ID.
REM this includes trailing spaces!
set UPDATE_NAME=update%GETGNUWIN32_ID%
set UPDATE_TRIALS=

:check_for_update
set UPDATE_FAILED=
set /a UPDATE_TRIALS="UPDATE_TRIALS + 1"

echo = 	
echo = Checking for signed update (attempt %UPDATE_TRIALS%^)...

del "%UPDATE_NAME%.*" 1>NUL 2>&1
if exist "%UPDATE_NAME%.*" (
	echo = 
	echo = Error: There was a problem checking for updates.
	echo = These files should not exist and could not be removed:
	dir /b "%UPDATE_NAME%.*"
	echo = 
	echo = Please make sure you have writable permissions to the
	echo = getgnuwin32 directory and all files therein.
	if not defined GNUWIN32_IGNORE_ERR (
		echo =
		echo = To continue on error you must specify the -c option.
		echo =
		echo = Download.bat cannot continue.
		set UPDATE_FAILED=TRUE
		goto bye
	)
	echo =
	echo = Continuing anyway...
	set UPDATE_FAILED=TRUE
	goto update_check_failed
)

if defined GNUWIN32_VERBOSE (
	echo = requesting http://%GETGNUWIN32_SITE%/%UPDATE_NAME%.zip
)

bin\%WGETPROG% --no-cache http://%GETGNUWIN32_SITE%/%UPDATE_NAME%.zip 1>sf_diag.tmp 2>&1
REM set WGET_ERR=%ERRORLEVEL%
bin\sed -n "/ERROR 404/p" sf_diag.tmp > sf_404.tmp
REM "ERROR 404" is english text; we have no gettext support in wget 1.12.1-devel so this is ok for now.

bin\test -s sf_404.tmp
if errorlevel 0 if not errorlevel 1 (
	del sf_404.tmp 1>NUL 2>&1
	echo =
	echo = Error: No update found on server ( Error 404 ^).
	if defined GNUWIN32_VERBOSE (
		echo =
		echo = here's wget output:
		type sf_diag.tmp 2>NUL
	)
	if not defined GNUWIN32_IGNORE_ERR (
		echo =
		echo = To continue on error you must specify the -c option.
		echo =
		echo = Download.bat cannot continue.
		set UPDATE_FAILED=TRUE
		goto bye
	)
	echo =
	echo = Continuing anyway...
	set UPDATE_FAILED=TRUE
	goto update_check_finished
)

bin\test -s %UPDATE_NAME%.zip
if errorlevel 0 if not errorlevel 1 (
	if defined GNUWIN32_VERBOSE (
		echo = 
		echo = update zipfile found, checking for required signature file...
		echo = requesting http://%GETGNUWIN32_SITE%/%UPDATE_NAME%.sig
	)
	bin\%WGETPROG% --no-cache http://%GETGNUWIN32_SITE%/%UPDATE_NAME%.sig 1>sf_diag.tmp 2>&1
) else (
	if defined GNUWIN32_VERBOSE (
		echo =
		echo = test for %UPDATE_NAME%.zip failed. here's wget output:
		type sf_diag.tmp 2>NUL
	)
	set UPDATE_FAILED=TRUE
	goto update_check_failed
)

:verify_update

bin\test -s %UPDATE_NAME%.sig
if errorlevel 0 if not errorlevel 1 (
	echo.
	echo GetGnuWin32 signed update found, verifying signature...
	bin\openssl dgst -verify bin\getgnuwin32.key.pub -sha256 -signature %UPDATE_NAME%.sig %UPDATE_NAME%.zip
	if errorlevel 0 if not errorlevel 1 (
		echo.
		bin\unzip -t %UPDATE_NAME%.zip 1>NUL 2>&1
		if errorlevel 0 if not errorlevel 1 (
			REM unzip could still fail if we cannot (over)write, but we checked that earlier...
			REM unzip will overwrite files with +R attrib
			bin\unzip -o %UPDATE_NAME%.zip
		) else (
			set UPDATE_FAILED=TRUE
			echo =
			echo = unzip test archive %UPDATE_NAME%.zip failed
		)
	) else (
		echo.
		set UPDATE_FAILED=TRUE
		if defined GNUWIN32_VERBOSE (
			echo =
			echo = openssl verify failed, errorlevel %ERRORLEVEL%
		)
	)
) else (
	set UPDATE_FAILED=TRUE
	echo =
	echo = %UPDATE_NAME%.sig missing data.
	if defined GNUWIN32_VERBOSE (
		dir %UPDATE_NAME%.sig
		echo = here's wget output:
		type sf_diag.tmp 2>NUL
	)
)

if defined UPDATE_FAILED (
	if defined GNUWIN32_VERBOSE echo = UPDATE_FAILED == TRUE
	goto update_check_failed
) else (
	echo.
	echo GetGnuWin32 update successful.
)

bin\test -s override.bat
if errorlevel 0 if not errorlevel 1 (
	REM update has a new download script.
	if defined GNUWIN32_VERBOSE (
		echo = 
		echo = A batch file update was found (override.bat^). Executing...
@ECHO ON
	)
	override.bat -o %GETGNUWIN32_ARGV%
	REM execution should stop here do not use CALL command to execute override.bat
	goto bye
)
REM if we've reached this point there is no new batch code.

goto update_check_finished

:update_check_failed
if /i %UPDATE_TRIALS% LEQ 6 (
	echo = 
	echo = Update failed. Trying again...
	goto check_for_update
)
echo = 
echo = Error: Update check failed on every try.
if not defined GNUWIN32_IGNORE_ERR (
	echo = To continue on error you must specify the -c option.
	echo =
	echo = Download.bat cannot continue.
	goto bye
)
echo = Continuing anyway...

:update_check_finished

if defined GNUWIN32_UPDATE_ONLY (
	echo = 
	echo = Command line option -u specified and the update check has finished.
	echo = Quitting.
	goto bye
)

if defined GNUWIN32_VERBOSE echo = Sorting packages...
REM OUTPUT BIN / DEP / LIB / DOC .ZIP PACKAGES WITH ONLY THE MOST RECENT RELEASE DATE

FOR %%A IN (bin dep lib doc) DO (
	if not exist "%%A.lst" (
		echo =
		echo = Error: File missing: "%%A.lst"
		echo = 
		echo =
		echo = Probable solutions:
		echo = Check for updates.
		echo = Please make sure you have writable permissions to the
		echo = getgnuwin32 directory and all files therein.
		echo =
		echo = Download.bat cannot continue.
		goto bye
	)
	bin\%SORTPROG% -s -t " " -k 3,3nr -k 1,1Mr -k 2,2nr "%%A.lst" | bin\%SORTPROG% -s -t / -u -k 2,2 > "%%A.out"
)

REM MERGE BIN / DEP / LIB / DOC LISTS AND SORT CHRONOLOGICALLY
bin\%SORTPROG% -s -m *.out -t / -k 2,2 | bin\%SORTPROG% -k3,3n -k 1,1M -k2,2n > getgnuwin32.tmp


::
:: Before we continue, we have to check if getgnuwin32.tmp contains some
:: information. If yes, old file lists must be removed first.
::
bin\test -s getgnuwin32.tmp
if errorlevel 0 if not errorlevel 1 goto create_new_filelist

:getgnuwin32_empty
echo =
echo = Warning: New file list could not be created (probable parsing issue^).

if exist getgnuwin32.tmp del getgnuwin32.tmp
bin\test -s getgnuwin32.lst
if errorlevel 1 (
	echo =
	echo = Error: File missing or does not contain data: "getgnuwin32.lst"
	dir "getgnuwin32.lst"
	echo = 
	echo =
	echo = Probable solutions:
	echo = Check for updates.
	echo = Please make sure you have writable permissions to the
	echo = getgnuwin32 directory and all files therein.
	echo =
	echo = Download.bat cannot continue.
	goto bye
)
echo = This script will now use an existing getgnuwin32.lst instead.
echo =
goto clean_old_lists

::
:: Create a new getgnuwin32.lst and backup the old one
::
:create_new_filelist
if exist getgnuwin32.lst if not exist getgnuwin32.bak ren getgnuwin32.lst getgnuwin32.bak
if exist getgnuwin32.lst del getgnuwin32.lst
ren getgnuwin32.tmp getgnuwin32.lst
if defined GNUWIN32_UPDATE_ONLY goto bye

::
:: Clean up old file lists etc.
::
:clean_old_lists
set OLD_LISTS=filelist.txt packages.txt packlist.txt
FOR %%A IN (%OLD_LISTS%) DO (
	del "%%A.txt" 1>NUL 2>&1
	if exist "%%A.txt" (
		echo = 
		echo = Error: These files could not be removed:
		dir /b %OLD_LISTS%
		echo = 
		echo = Please make sure you have writable permissions to the
		echo = getgnuwin32 directory and all files therein.
		echo =
		echo = Download.bat cannot continue.
		goto bye
	)
)

::
:: Extract the filelist from temporary file getgnuwin32.lst
::
:extract_filelist
bin\sed -T "s/[^\/]*//" getgnuwin32.lst >packlist.txt
if defined GNUWIN32_VERBOSE echo = Extracting filelist...

::
:: Add/Remove packages
::

:: Remove all comments, whitespace, blank lines, from include/exclude.txt

if exist include.txt bin\sed -T "s/#.*//;s/^[ \d009]*//;s/[ \d009]*$//;/^$/ d" include.txt > include.tmp
if exist exclude.txt bin\sed -T "s/#.*//;s/^[ \d009]*//;s/[ \d009]*$//;/^$/ d" exclude.txt > exclude.tmp

:: Remove packages listed in exclude.tmp
if defined GNUWIN32_VERBOSE echo = Removing any packages listed in exclude.txt

del packlist.tmp exclude.sed 1>NUL 2>&1
bin\test -s exclude.tmp
if errorlevel 0 if not errorlevel 1 (
	bin\sed -f bin\sanitize.sed -e "s/^\(.*\)$/\/^\\\/\1\\\/\/Id/"  exclude.tmp > exclude.sed
	bin\test -s exclude.sed
	if errorlevel 0 if not errorlevel 1 (
		bin\sed -f exclude.sed  packlist.txt > packlist.tmp
		bin\test -s packlist.tmp
		if errorlevel 0 if not errorlevel 1 (
			move /y packlist.tmp packlist.txt 1>NUL
			REM echo Package exclusions listed in exclude.txt have been removed from packlist.txt
		)
	)
)
del exclude.tmp exclude.sed 1>NUL 2>&1

:: Add packages listed in include.tmp
if defined GNUWIN32_VERBOSE echo = Adding any packages listed in include.txt

del packlist.tmp found.tmp include.sed remove.sed 1>NUL 2>&1
bin\test -s include.tmp
if errorlevel 0 if not errorlevel 1 (
	bin\sed -f bin\sanitize.sed -e "s/^\(.*\)$/\/\\\/\1\\\.zip\/Ip/"  include.tmp > include.sed
	bin\test -s include.sed
	if errorlevel 0 if not errorlevel 1 (
		FOR %%A IN (bin dep lib doc) DO (
			bin\test -s "%%A.tmp"
			if errorlevel 0 if not errorlevel 1 (
				REM sed as grep to find packages listed in include.txt (all versions of packages should be in bin|dep|lib|doc.tmp)
				bin\sed -n -e "s/[^\/]*//" -f include.sed "%%A.tmp" >> found.tmp
			)
		)
		
		REM echo pause before cat
		REM pause
		REM cat found.tmp
		bin\test -s found.tmp
		if errorlevel 0 if not errorlevel 1 (
			REM echo hi
			REM dir found.tmp
			REM bin\%SORTPROG% -u found.tmp
			REM echo /arc/5.21j-1/arc-5.21j-1-bin.zip | sed -n "s/^\/\([^/]*\)\/.*\(bin\|doc\|lib\|dep\)\.zip/\/\\\/\1\\\/\.\*\2\\\.zip\/Id/Ip"
			bin\sed -n "s/^\/\([^/]*\)\/.*\(bin\|doc\|lib\|dep\)\.zip/\/\\\/\1\\\/\.\*\2\\\.zip\/Id/Ip" found.tmp > remove.sed
			bin\test -s remove.sed
			if errorlevel 0 if not errorlevel 1 (
				REM sed as grep -v to remove from packlist.txt more current versions of files than those listed in include.txt
				REM ie if an outdated doc for wget is requested we remove most any wget doc file currently in packlist
				REM user requests include wget-1.9-1-doc; we found /wget/1.9-1/wget-1.9-1-doc.zip
				REM so remove /\/wget\/.*doc\.zip/Id
				REM which eg removes /wget/1.11.4-1/wget-1.11.4-1-doc.zip
				bin\sed -f remove.sed packlist.txt > packlist.tmp
				REM do stuff?
			)
		)
	)
)
del include.tmp include.sed remove.sed 1>NUL 2>&1

bin\test -s packlist.tmp
if errorlevel 1 (
REM if nothing was removed from packlist.txt, there is no information in packlist.tmp 
REM in any case we need packlist.tmp, so rename packlist.txt as .tmp
	move /y packlist.txt packlist.tmp 1>NUL
)

REM however a user specific include request does not always result in removal, so handle the include merge separately:
bin\test -s found.tmp
if errorlevel 0 if not errorlevel 1 (
REM append found.tmp to packlist
REM colons are buggy for comments
REM using copy command and + operator for append will stick an eof. use sed instead
	bin\sed -T -n p packlist.tmp found.tmp > packlist.txt
) else (
	move /y packlist.tmp packlist.txt 1>NUL
)
del found.tmp >NUL 2>&1

REM delete from packlist any blank lines or lines which contain any of the characters \ : * ? " < > |
REM also remove lines which contain string %%
if defined GNUWIN32_VERBOSE echo = Sanitizing packlist...
bin\sed -T -e "/[\x5c\x3a\x2a\x3f\x22\x3c\x3e\x7c]/d" -e "/\x25\x25/d" -e "/^$/d" packlist.txt > packlist.tmp

if defined GNUWIN32_VERBOSE echo = Parsing packlist filenames...
bin\sed -n "s/.*\/\(.*\(bin\|doc\|lib\|dep\)\.zip\)[ \d009]*/\1/p" packlist.tmp > filelist.txt
REM bin\sed -n "s/[^/]*\/\(.*\(bin\|doc\|lib\|dep\)\.zip\)[ \d009]*/\1/p" packlist.tmp > packlist.txt
bin\sed -n "s/[^/]*\(\/.*\(bin\|doc\|lib\|dep\)\.zip\)[ \d009]*/\1/p" packlist.tmp > packlist.txt

del packlist.tmp >NUL 2>&1
REM echo arf
REM pause

bin\test -s filelist.txt
if errorlevel 1 (
	echo =
	echo = Error: failed to parse filenames
	echo =
	echo = Please make sure you have writable permissions to the
	echo = getgnuwin32 directory and all files therein.
	echo =
	echo = Download.bat cannot continue.
	goto bye
)


::
:: Download the desired files from the server
::
:download_packages
if defined GNUWIN32_UPDATE_ONLY goto bye
if not exist packages goto makepackages
if not exist oldpacks goto makeoldpacks
REM merge directories together.
move /y packages\*.* oldpacks >NUL 2>&1
dir /b packages >error_moving_to_oldpacks.tmp
if not exist error_moving_to_oldpacks.tmp (
	echo =
	echo = Error: failed to create temporary file:
	echo = error_moving_to_oldpacks.tmp
	echo =
	echo = Please make sure you have writable permissions to the
	echo = getgnuwin32 directory and all files therein.
	echo =
	echo = Download.bat cannot continue.
	goto bye
)

bin\test -s error_moving_to_oldpacks.tmp
if errorlevel 0 if not errorlevel 1 (
	echo =
	echo = Error: failed to merge directories packages and oldpacks
	echo = Suggested: move files in packages directory to oldpacks
	echo =
	echo = Please make sure you have writable permissions to the
	echo = getgnuwin32 directory and all files therein.
	echo =
	echo = Download.bat cannot continue.
	goto bye
)
del error_moving_to_oldpacks.tmp >NUL 2>&1

goto makepackages

:makeoldpacks
ren packages oldpacks
if exist packages (
	echo =
	echo = Error: failed to rename packages directory
	echo =
	echo = Please make sure you have writable permissions to the
	echo = getgnuwin32 directory and all files therein.
	echo =
	echo = Download.bat cannot continue.
	goto bye
)

:makepackages
rmdir /s /q packages >NUL 2>&1
if exist packages (
	if errorlevel 1 (
		echo =
		echo = Error: failed to remove packages directory
		echo =
		echo = Please make sure you have writable permissions to the
		echo = getgnuwin32 directory and all files therein.
		echo =
		echo = Download.bat cannot continue.
		goto bye
	)
)
mkdir packages
if not exist packages (
	if errorlevel 1 (
		echo =
		echo = Error: failed to create packages directory
		echo =
		echo = Please make sure you have writable permissions to the
		echo = getgnuwin32 directory and all files therein.
		echo =
		echo = Download.bat cannot continue.
		goto bye
	)
)

:download
echo %GNUWIN32_LOAD% >loadlist.tmp
bin\sed -T "s/  *$//;s/ /\n/g" loadlist.tmp >loadlist.txt
del loadlist.tmp
REM cat loadlist.txt
REM pause

REM we can't continue without a list of packages and hashes.
FOR %%A IN (packlist.txt packages.sha) DO (
	bin\test -s "%%A"
	if errorlevel 1 (
		echo =
		echo = Error: File missing or does not contain data: "%%A"
		dir "%%A"
		echo = 
		echo =
		echo = Probable solutions:
		echo = Check for updates.
		echo = Please make sure you have writable permissions to the
		echo = getgnuwin32 directory and all files therein.
		echo =
		echo = Download.bat cannot continue.
		goto bye
	)
)

if defined GNUWIN32_VERBOSE echo = Switching to package directory...
cd packages
if errorlevel 1 (
	echo =
	echo = Error: failed to switch to packages directory
	echo =
	echo = Please make sure you have writable permissions to the
	echo = getgnuwin32 directory and all files therein.
	echo =
	echo = Download.bat cannot continue.
	goto bye
)

REM remove temporary files in package directory (current directory)
FOR %%A IN (tmp txt) DO (
	del "*.%%A" 1>NUL 2>&1
	if exist "*.%%A" (
		echo = 
		echo = Error: These files in the package directory could not be removed:
		dir /b "*.%%A"
		echo = 
		echo = Please make sure you have writable permissions to the
		echo = getgnuwin32 directory and all files therein.
		echo =
		echo = Download.bat cannot continue.
		goto bye
	)
)

set GNUWIN32_OLD_COUNT=
if exist ..\oldpacks (
	echo =
	echo = Validating hashes of previously downloaded packages.
	echo = Only invalid or missing packages will be downloaded.
	echo =
	set GNUWIN32_OLD_COUNT=0
	REM validate hashes of previously downloaded packages
	FOR /f "tokens=*" %%f IN (..\packlist.txt) DO (
		REM echo "f: %%f"
		REM echo "nf: %%~nxf"
		REM quotes necessary for %%f for cmd shell parsing: to ignore &, ^ etc.
		if exist "..\oldpacks\%%~nxf" (
			..\bin\findhash -b "..\oldpacks\%%~nxf" ..\packages.sha >NUL
			if errorlevel 0 if not errorlevel 1 (
				REM 0 (PASS) if successful [and hash exists in hashlist]
				move /y "..\oldpacks\%%~nxf" . >NUL 2>&1
				if exist "%%~nxf" (
					REM echo "%%~nxf"| ..\bin\sed -n "s/^\d034\(.*\)\d034$/\1/p" >> hash_pass.txt
					set /a GNUWIN32_OLD_COUNT+=1
				)
			) else if errorlevel 3 (
				REM >= 3 if there's an internal error (openssl, memory, file, assert)
				goto findhash_failed
			)
		)
	)
)
if defined GNUWIN32_OLD_COUNT (
	echo = Found %GNUWIN32_OLD_COUNT% previously downloaded valid packages.
) else (
	set GNUWIN32_OLD_COUNT=0
)

REM count the total number of packages in the list
set GNUWIN32_TMP_COUNT=0
..\bin\sed -n "$=" ..\packlist.txt > count.tmp
..\bin\test -s count.tmp
if errorlevel 1 (
	goto count_tmp_error
)
set /p GNUWIN32_TMP_COUNT=<count.tmp
del count.tmp >NUL 2>&1
if not defined GNUWIN32_TMP_COUNT set GNUWIN32_TMP_COUNT=0
if /i %GNUWIN32_OLD_COUNT% GTR %GNUWIN32_TMP_COUNT% (
	echo ERROR: whoops, this shouldn't have happened:
	echo GNUWIN32_OLD_COUNT ^> GNUWIN32_TMP_COUNT
	echo GNUWIN32_OLD_COUNT == %GNUWIN32_OLD_COUNT%
	echo GNUWIN32_TMP_COUNT == %GNUWIN32_TMP_COUNT%
)
REM subtract the total number of packages by the number already downloaded and verified
set /a GNUWIN32_NEW_COUNT="GNUWIN32_TMP_COUNT - GNUWIN32_OLD_COUNT"
REM new count == packages to be downloaded and verified
REM old count == packages already downloaded and verified

echo =
echo = Downloading and verifying %GNUWIN32_NEW_COUNT% new packages from sourceforge.net...
echo = 
echo = This will take some time, be patient. No interaction is needed.
echo = If you have to stop this process (CTRL + C^) it can pickup where it left off.
echo =


REM download new files, validate hashes.
REM at this point only files that have valid hashes exist in the directory.
set countfail=0
set DOWNLOAD_COUNT=0
set PERCENT_DONE=5
set TMP_NUMBER=0
	rem set /a PERCENTAGE="GNUWIN32_NEW_COUNT / PERCENT_DONE"
	REM echo %DOWNLOAD_COUNT%
	REM echo %GNUWIN32_NEW_COUNT% %PERCENT_DONE% %PERCENTAGE%
FOR /f "tokens=*" %%f IN (..\packlist.txt) DO (
	REM echo "nf: %%~nxf"
	REM echo "f: %%f"
	REM quotes necessary for %%f for cmd shell parsing: to ignore &, ^ etc.
	if not exist "%%~nxf" (
		FOR /f %%m IN (..\loadlist.txt) DO (
			if not exist "%%~nxf" (
				..\bin\%WGETPROG% "%%m%%f" 1>NUL 2>&1
				if exist "%%~nxf" (
					..\bin\findhash -b "%%~nxf" ..\packages.sha >NUL
					if errorlevel 1 if not errorlevel 2 (
						REM 1 (FAIL) if file's computed hash is different from its hash in hashfile
						del "%%~nxf" >NUL 2>&1
					) else if errorlevel 3 (
						REM >= 3 if there's an internal error (openssl, memory, file, assert)
						goto findhash_failed
					)
				)
			)
		)
		call :print_percentage
	)
	
	..\bin\test -s "%%~nxf"
	if errorlevel 0 if not errorlevel 1 (
		..\bin\findhash -b "%%~nxf" ..\packages.sha >NUL
		if errorlevel 0 if not errorlevel 1 (
			REM 0 (PASS) if successful [and hash exists in hashlist]
			echo "%%~nxf"| ..\bin\sed -n "s/^\d034\(.*\)\d034$/\1/p" >> hash_pass.txt
		) else if errorlevel 1 if not errorlevel 2 (
			REM 1 (FAIL) if file's computed hash is different from its hash in hashfile
			echo "%%~nxf"| ..\bin\sed -n "s/^\d034\(.*\)\d034$/\1/p" >> hash_fail.txt
			del "%%~nxf" >NUL 2>&1
		) else if errorlevel 2 if not errorlevel 3 (
			REM 2 (MISSING) if file's name is not found as an entry in hashfile
			echo "%%~nxf"| ..\bin\sed -n "s/^\d034\(.*\)\d034$/\1/p" >> hash_missing.txt
		) else (
			REM >= 3 if there's an internal error (openssl, memory, file, assert)
			goto findhash_failed
		)
	) else (
		echo "%%~nxf"| ..\bin\sed -n "s/^\d034\(.*\)\d034$/\1/p" >> downfail.txt
		set /a countfail+=1
	)
	
)
REM 100 percent done: finished downloading and verifying packages.
echo = 
echo = 

set temp_variable=0
..\bin\test -s hash_missing.txt
if errorlevel 0 if not errorlevel 1 (
	..\bin\sed -n "$=" hash_missing.txt > count.tmp
	..\bin\test -s count.tmp
	if errorlevel 1 (
		goto count_tmp_error
	)
	set /p temp_variable=<count.tmp
	del count.tmp >NUL 2>&1
	if not defined temp_variable (
		set temp_variable=0
	)
)

if /i %temp_variable% NEQ 0 (
	echo =
	echo = Warning: %temp_variable% files are missing a hash entry (hash_missing.txt^).
	echo = To install files missing a hash entry, please run install with the -m option.
	echo =
)

if /i %countfail% NEQ 0 (
	echo=
	echo = Warning: %countfail% files were not downloaded completely or are corrupted.
	echo = The failed files have been removed (see downfail.txt, hash_fail.txt^)
	echo = Run download.bat again to try downloading the failed files again.
	echo = Also you could try downloading from different sourceforge mirrors:
	echo = download.bat surfnet switch
	echo =
) else (
	echo =
	echo = It appears everything was a success.
	echo =
)

set countvalid=0
..\bin\test -s hash_pass.txt
if errorlevel 0 if not errorlevel 1 (
	..\bin\sed -n "$=" hash_pass.txt > count.tmp
	..\bin\test -s count.tmp
	if errorlevel 1 (
		goto count_tmp_error
	)
	set /p countvalid=<count.tmp
	del count.tmp >NUL 2>&1
	if not defined countvalid (
		set countvalid=0
	)
)

if %countvalid% NEQ 0 (
	echo = There are %countvalid% files that have been verified and are ready to be unzipped.
	echo = Run install.bat to install verified packages in default temp gnuwin32 subdir.
	echo = To instead install to a preferred directory run like so, eg:
	echo = install C:\gnuwin32
	echo =
	echo = Run install --help for more information
) else (
	echo = Uh oh. No verified files have been found.
	if exist hash_missing.txt (
		echo =
		echo = You can run install with the -m switch to install unverified files.
		echo = But really, it's best to re-run this script to verify the packages.
		echo =
	)
	echo = Quitting...
	echo =
)

cd ..
del loadlist.txt

::
:: Clean up things
::
if not exist oldpacks goto bye
REM dir /b oldpacks >oldpacks.txt
REM bin\test -s oldpacks.txt
REM if errorlevel 1 rmdir /s /q oldpacks
rmdir /s /q oldpacks >NUL 2>&1
if exist oldpacks (
	echo = !
	echo = Warning: The oldpacks directory could not be removed. Please remove it manually.
	echo = !
)
if exist oldpacks.txt del oldpacks.txt
if exist oldpacks.txt (
	echo = !
	echo = Warning: oldpacks.txt could not be removed. Please remove it manually.
	echo = !
)

goto bye

:count_tmp_error
echo =
echo = Error: File missing or does not contain data: "count.tmp"
dir "count.tmp"
echo = 
echo = Please make sure you have writable permissions to the
echo = getgnuwin32 directory and all files therein.
echo =
echo = Download.bat cannot continue.
goto bye

:findhash_failed
echo = 
echo = Error: findhash utility failed, errorlevel %ERRORLEVEL%
echo = If this problem persists, report it with the findhash error message above.
echo = 
echo = ^<raysatiro@users.sourceforge.net^>
echo =
echo = Download.bat cannot continue without verifying packages.
goto bye

:print_percentage
set /a DOWNLOAD_COUNT+=1
REM echo percentdone%PERCENT_DONE% gnc%GNUWIN32_NEW_COUNT%
set /a TMP_NUMBER="(GNUWIN32_NEW_COUNT/20)*(PERCENT_DONE/5)"
REM echo %DOWNLOAD_COUNT% %TMP_NUMBER%
if /i %DOWNLOAD_COUNT% GTR %TMP_NUMBER% (
	echo = %PERCENT_DONE% percent done...
	set /a PERCENT_DONE+=5
)
goto :EOF

:bye
endlocal
pause
