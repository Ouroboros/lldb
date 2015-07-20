-------------------------------------------------------------------

Maintaining a GnuWin32 package list and installation directory

author : Mathias Michaelis <michaelis@tcnet.ch>
         Jay Satiro <raysatiro@users.sourceforge.net>
date   : November 15, 2009
version: 0.6.3

Copyright (c) 2009 by Mathias Michaelis, <michaelis@tcnet.ch>

-------------------------------------------------------------------

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.2
or any later version published by the Free Software Foundation;
with no Invariant Sections, no Front-Cover Texts, and no Back-Cover
Texts.  A copy of the license is included in the file LICENSE.TXT


Abstract
--------

The http://gnuwin32.sourceforge.net project makes available a huge
number of packages for download. But every and each single package
must be chosen carefully, because there are new as well as old
versions on the server, there are source, binary, documentation and
installer packages etc. Even worse: You must download and install
each and every package on its own. This is a lot of work!

On the other hand, maintaining a small local package list of only the
newest binaries and documentation can very easily be automated. This is
the subject of this software package.


Important notes for developers
-----------------------------

Although it isn't necessary to distinguish between a runnable batch
script and its source, I do so. Therefore, there exist a runnable
package release and a source package release for every version of
GetGnuWin32.

If you have the source release, you will find, among others, a file
INSTALL.TXT and a file MAKE.BAT. Both are not present in the runnable
release. Also, almost all batch scripts and text files (including this
one) contain some macros like RELEASE_NUMBER etc., surrounded by
dollar signs.

The file MAKE.BAT constructs a runnable release by copying all needed
files into a winrar archive, into a zip archive or simply into a
subfolder, depending on which archiver it finds on the host it runs.
Then, MAKE.BAT replaces all macros by appropriate values.

How MAKE.BAT works is described in the file INSTALL.TXT.


Acknowledgments
---------------

Jay Satiro <raysatiro@users.sourceforge.net>
  Oct 07, 2009 (0.6.22): He did all programming work. He thus is know
  a co-author of this project.

Jay Satiro <raysatiro@users.sourceforge.net>
  Feb 19, 2009 (0.6.21): Jay Satiro <raysatiro@users.sourceforge.net>
  remarked the unix file ending bug instantly and let me know about
  it.

Jay Satiro <raysatiro@users.sourceforge.net>
  Oct 07, 2009 (0.6.20): He has done all programming mentioned here.
  Due to his work the getgnuwin32 project is alive again.

Ruslan Gainutdinov <ruslanfg@gmail.com>
  Nov 25, 2006 (0.6.17): Due to changes of the web interface at
  sourceforge.net, download.bat didn't work anymore. Ruslan realized
  that, immediately adopted download.bat and sent me his version.

Martin Mewes <mm@mewes.tv>
  March 3, 2006 (0.6.12): Martin succeeded to implement a little more
  comfort. Due to his proposal following improvements were made: You
  can now specify a list of mirrors both with the GNUWIN32_MIRROR
  environment variable or on the command line. You can specify a
  verbose switch -v.

Amores Perros <perryrapp@gmail.com>
  Jan 16, 2006 (0.6.11): I am not a native English speaker -- so
  Amores corrected many clerical errors and suggested some
  clarifications.

Yesudeep J Mangalapilly <yesudeep@gmail.com>
  Dec 25, 2005 (0.6.8): Yesudeep J Mangalapilly offered me a Christmas
  present.  He sent me a patch that corrected some misspellings,
  improved status messages and added a feature to create links to .hlp
  and .chm files within the Windows Start Menu.

Ryan McCullough <rmccullough@gmail.com>
  Nov 21, 2005 (0.6.7): Ryan told me that the layout of the gnuwin32
  web site has changed so that DOWNLOAD.BAT didn't work anymore. Also,
  wget-1.10.2 made troubles, so I had to use the tool from where I was
  told before by Mark Gardner.

Mark Gardner <mark.gardner@bms.com>
  Nov 2, 2005 (0.6.7): He proposed to use wget-1.10.2 and gave me the
  tip to get it from http://xoomer.virgilio.it/hherold/.

Ryan McCullough <rmccullough@gmail.com>
  April 18, 2005 (0.6.5): Thanks for your issue, that DOWNLOAD.BAT
  sometimes fetches some old dlls out of one of the xyz-deb.zip
  packages and overwrites already extracted new ones. And even more:
  Thanks for your server place at http://getgnuwin32.nooblink.com.  

Jochen Kuhnhenn <kuhnhenn@web.de>
  April 12, 2005 (0.6.4) The idea to have a wget.ini file within the
  bin directory that one can adapt for special purposes.
  
Paul Moore <pf_moore@yahoo.co.uk>
  March 5, 2005 (0.6.1 and 0.6.2) Great starting aid! Thanks for your
  issues!



Introduction: How it works
--------------------------

Only the newest binary, library and documentation packages are
downloaded from one of the servers

http://<mirror>.dl.sourceforge.net/sourceforge/gnuwin32/

No source packages and no installers are downloaded.  It makes no
sense to install every and each package for its own.  Installation is
simply done by extracting the archives, some overhead to keep some
directories in the gnuwin32 tree tidy, and by updating the info tree.

This idea sounds easy -- but in order to do as written above, there
are some problems that must be resolved: How do you automate the
process of choosing the desired packages out of the more than a
thousand packages on the server?

The BATCH script DOWNLOAD.BAT handles this: It retrieves a complete
file list from the server

http://sourceforge.net/projects/gnuwin32/files/

and automaticaly edit this list by means of an old gnuwin32 "sed"
command. The revised file list then is used to start the download.
Only packages that are not found in the directory PACKAGES are
downloaded. Note that a list of the most recent zips, organized
chronologically, as of October 01 2009 is listed in getgnuwin32.lst.
This list is (read: should be) updated when you run DOWNLOAD.BAT.

The BATCH script INSTALL.BAT then unpacks all packages whose name is
found in the revised file list into the directory GNUWIN32. Special
care is taken when unpacking the subdirectories DOC and CONTRIB. Then,
the texinfo system is updated. INSTALL.BAT also copies UPDATE-LINK.BAT
into the GNUWIN32 directory and adopts it. With UPDATE-LINK.BAT you
can, among others, create a Windows Start Menu as soon as you have
moved GNUWIN32 to the definite place.

If INSTALL.BAT doesn't find any file list, it creates one from the
newest packages found in PACKAGES.

Both BATCH scripts call programs that can be found in the BIN
directory. Here, a minimal GNUWIN32 environment is already present.


Installation
------------

In order to get installed the GnuWin32 environment, you may have to
edit the file DOWNLOAD.BAT: Here you can choose the mirror nearest to
you. When you don't have a running wget environment (the command
'set WGETRC' on the command prompt says 'WGETRC is not defined'), then
you may edit the local bin\wget.ini sample file. This is especially
important if your connection to the world wide web depends on some
proxy servers.

Then do the following:

1)  In the start menu click "run", type "cmd" and cd in the directory
    where DOWNLOAD.BAT, INSTALL.BAT etc. are located.

2)  To specify a server, enter "SET GNUWIN32_MIRROR=<mirror-list>",
    where <mirror-list> is a space separated list of <mirror>s and a
    <mirror> can be one of the following names:

    aleron   (US)
    belnet   (BE)
    easynews (US)
    heanet   (IE)
    internap (US)
    jaist    (JP)
    keihanna (JP)
    kent     (UK)
    mesh     (DE)
    optusnet (AU)
    ovh      (FR)
    puzzle   (CH)
    switch   (CH)
    umn      (US)
    unc      (US)
    voxel    (US)

    Example: SET GNUWIN32_MIRROR="umn unc voxel". If you don't set
    anything, then umn is used.

3)  To download the newest packages, run DOWNLOAD.BAT. You may first
    edit the file EXCLUDE.TXT to prevent downloading some obsolet
    packages or those packages that are excluded by other packages.

4)  To extract the newest archives, run INSTALL.BAT

5)  As administrator, you may copy the subdirectory GNUWIN32 that is
    created or updated by INSTALL.BAT at any desired place, e.g. at

    C:\Program Files

    Note that when you copy the files within an NTFS file system, the
    access rights are inherited from the destination directory, what
    in general is what you want. However, when you move the directory
    instead of copying it, then the access rights, ownerships etc. are
    not changed.

    After copying the gnuwin32 subdirectory, you probably will have to
    run the BATCH script GNUWIN32\update-links.bat. It adjusts all
    Windows shortcuts and creates a start menu folder. The first time
    it is run, it creates a protocol file GNUWIN32\update-links.txt
    that is reused for performance reasons whenever the GNUWIN32
    folder is moved or copied again and update-links.bat is restarted.

6)  The subdirectory "GNUWIN32\Start Menu" contains links to the
    documetation files of the various projects. There is also a link
    called "gnuwin32.lnk" which points to %windir%\system32\cmd.exe
    and opens a command window with all essential environment variable
    set or adopted.
    
    You can move this links into your "Start Menu" directories.

7)  If you want to have your gnuwin32 environment ready whenever you
    open a cmd window, you have to follow steps 8) and 9). However,
    there are some disadvantages to have that: There are many programs
    like "sort.exe", "link.exe" etc. that exists already on a brand
    new Windows installation or in other installed environment. So,
    which of these even named program is finally started when you type
    their name depends on where you are in the directory hierarchy and
    on the order of the entries within the PATH environment variable.

    Therefore, I recommend to skip steps 8) and 9): There are two
    other methods of accessing your gnuwin32 environment:

    a) Use the shortcut gnuwin32.lnk in the Start Menue (see step 6).
       It opens a cmd window and calls the BATCH script
       set_gnuwin32.bat that is located in your gnuwin32\bin
       directory. This script adopts the PATH environment variable and
       all other environment variables that are needed. You may edit
       set_gnuwin32.bat to meet your needs.

    b) Copy set_gnuwin32.bat at a place that is listed in your PATH
       environment variable, e.g. into %windir%. If you are within a
       command window, type "set_gnuwin32" to setup the environment for
       gnuwin32. If you don't need the gnuwin32 tools anymore, type
       "set_gnuwin32 /u". You can also need the switch /s to define
       the environment variable SHELL_ENVIRONMENT and set the window
       title. To define the LANG and LANGUAGE environment variables,
       use /l. To get help, type "set_gnuwin32 /?".

    The idea of both possibilities, a) and b), is that there may exist
    several environments, e.g. a software development kit, Microsoft
    Windows Resource Kit etc. You can activate each environment by
    clicking on the corresponding shortcut or by issuing commands
    like set_sdk.bat, set_wrk.bat, set_gnuwin32.bat etc. With the /u
    switch you can leave again.

    The switch /s of those commands is mainly thought for the use with
    Windows shortcuts: The window title then corresponds with the
    shortcut that opened the window. The SHELL_ENVIRONMENT variable
    can be used if some BATCH script is executed automatically every
    time a new command windows is opened. With SHELL_ENVIRONMENT you
    can do some special treatment on certain environments.
    
    If the automatically executed BATCH script is called before the
    set_xyz.bat file, you can adopt StartCmd.bat, a BATCH script
    located in gnuwin32\bin that can parse the %CMDCMDLINE%
    environment variable, find the /s switch and define
    SHELL_ENVIRONMENT before set_xyz.bat is actually called.

8)  As mentioned above (step 7), this and the next step are optional.
    As administrator, set the following environment variables:

    set INFOPATH=C:/Program Files/GnuWin32/info
    set WGETRC=C:\Program Files\GnuWin32\etc\wgetrc
    set A2PS_CONFIG=C:\Program Files\GnuWin32\etc\a2ps.cfg
    set TEMP=<Directory for temporary files>
    set TMP=<Directory for temporary files>
    set TMPDIR=<Directory for temporary files>
    set LANG=EN
    set LANGUAGE=EN

    If you have modified the local bin\wget.ini file, then you
    probably will have to copy it to the %WGETRC% destination defined
    above to use it further on.

9)  As administrator, put the path C:\Program Files\gnuwin32\bin into
    the PATH environment variable.
    
    Note: There exist a sort.exe program already in the
    %WINDIR%\system32 folder. To prevent using this one further on,
    put the gnuwin32 Path (C:\Program Files\gnuwin32\bin) before the
    %WINDIR%\system32 path within the PATH environment variable.
    
    Note: The program link.exe whithin your gnuwin32 path may collide
    with linkers from locally installed development tools (especially
    with the one from the Microsoft Visual Development Studio). To
    prevent this, rename the gnuwin32 link.exe to gnulink.exe. If you
    want to create a windows shortcut, use mkshortcut.exe from the
    cygwin package.

    Note: To get rid of those and similar collisions, read step 7).
    
10) If you try out your brand new gnuwin32 environment by opening a
    CMD window and type 'info', you may get the message

    info: Terminal type `dumb' is not smart enough to run Info

    This is a bug or feature that appeared in the texinfo package
    somewhere after version 4.2. If this happens, you must get the
    version 4.2 or below. Unfortunately, there exist no old gnuwin32
    texinfo packages. So you must get your package from another place.
    
    Also you may not be able to create disk images by means of dd.exe.
    
    The directory cygwin-info contains a minimal cygwin environment in
    order to run info.exe and a readme.txt file that describes the
    usage of dd.exe. The readme.txt file describes also how you can
    get the packages yourself and how you install the environment.

11) To get up a2ps.exe, you must adjust some settings within your
    gnuwin32\etc\a2ps.cfg and gnuwin32\etc\a2ps-site.cfg file. Replace
    all paths with absolute paths. Thereby, the path names must not
    contain white spaces.  If so, use 8.3 path name convention.
    Further, the absolute paths must begin with a / path
    delimiter, NOT with a drive letter. All path delimiters '\' have to
    be replaced by '/'.
    
    Normally this means that you must change the last line in a2ps.cfg
    
    from "Include: a2ps-site.cfg"
    to   "Include: /Progra~1/gnuwin32/etc/a2ps-site.cfg"
    
    and (maybe within a2ps-site.cfg) the line that begins with
    "LibraryPath" to
    
    LibraryPath: /progra~1/gnuwin32/share/a2ps/sheets;\
    /progra~1/gnuwin32/share/a2ps/ps;\
    /progra~1/gnuwin32/share/a2ps/encoding;\
    /progra~1/gnuwin32/share/a2ps/afm;\
    /progra~1/gnuwin32/share/ogonkify/afm;\
    /progra~1/gnuwin32/share/a2ps/ppd;\
    /progra~1/gnuwin32/share/a2ps/fonts;\
    /progra~1/gnuwin32/share/ogonkify/fonts;\
    /progra~1/gnuwin32/share/a2ps
    
    Next, if no running file.exe exists on your system, replace the
    line containing
    
    FileCommand: file.exe -L
    
    within a2ps-site.cfg by the line
    
    FileCommand: file.bat 
    
    and then create the .BAT file gnuwin32\bin\file.bat which should
    contain
    
    @echo off echo text/plain

