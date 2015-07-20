-------------------------------------------------------------------

Work around some bugs within the gnuwin32 project

author : Mathias Michaelis <michaelis@tcnet.ch>
date   : Feb 19, 2009
version: 0.6.21

Copyright (c) 2009 by Mathias Michaelis, <michaelis@tcnet.ch>

-------------------------------------------------------------------

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.2
or any later version published by the Free Software Foundation;
with no Invariant Sections, no Front-Cover Texts, and no Back-Cover
Texts.  A copy of the license is included in the file LICENSE.TXT


Acknowledgments
---------------

See file README.TXT in the parent directory. Thanks to all who advised
me, gave me some hints, sent me some patches or encouraged me to go
on!


Abstract
--------

Some bugs or unadapted features decreases the worth and usability of
the GnuWin32 project. In the moment, I know the following issues:

1) info.exe does not work in a DOS or CMD window under MS Windows.

2) With dd.exe, no devices (such as a:) can be accessed.

Issue 1) is simply and dirty worked around by using the corresponding
program from the cygwin project.

Issue 2) can be treated by specifying the devices with the correct
syntax, as shown below.


info.exe: The issue
-------------------

Some or all versions after 4.2 of the texinfo package complain about
the stupidity of the CMD windows within Microsoft Windows:

info: Terminal type `dumb' is not smart enough to run Info

Since this happens to all texinfo packages newer than version 4.2, you
must keep the version of texinfo at 4.2-x. Unfortunately there exists
no old texinfo package in the gnuwin32 archive. So you must use the
package from another archive, e.g. from the cygwin archive.


info.exe: The work around
-------------------------

In order to install the minimal required files (without all of the
cygwin overhead, installers etc.), go on like this:

1) Go to http://cygwin.com/mirrors.html and select the mirror that
   is closest to you.

2) From this server, select the following archives:

   url-to-mirror/release/texinfo/texinfo-4.2-4.tar.bz2
   url-to-mirror/release/cygwin/cygwin-1.5.12-1.tar.bz2
   url-to-mirror/release/libiconv/libiconv2/libiconv2-1.9.2-1.tar.bz2

   Thereby, 'url-to-mirror' can be someting like

   ftp://mirror.switch.ch/mirror/cygwin/
   http://ftp.gwdg.de/pub/linux/sources.redhat.com/cygwin/
   ftp://ftp.cise.ufl.edu/pub/mirrors/cygwin/

   Note that you must not download the newest texinfo archive (at
   the moment texinfo-4.7-2.tar.bz2), because the new versions all
   bring up the message 'info: Terminal type `dumb' is not smart
   enough to run Info' and end immediately. On the other hand, you
   may try to download the newest cygwin and libiconv2 packages.

3) From the downloaded archives, unpack the following files and
   put them into a directory that is listed within the PATH
   environment variable:

   texinfo-4.2-4.tar.bz2     /usr/bin/info.exe
   cygwin-1.5.12-1.tar.bz2   /usr/bin/cygwin1.dll
   libiconv2-1.9.2-1.tar.bz2 /usr/bin/cygiconv-2.dll

4) Have fun with info!


dd.exe: The issue
-----------------

All of the helpful information in this section are extracted from an
email written by Tor Lillqvist <tml@iki.fi>.

As a puritan DOSian, one wants to create a disk image by using

dd if=a: of=disk.img bs=1k count=1440

Here, dd.exe will complain that 'a:' is a directory and not a device.  
And indeed: 'a:' is the current directory of the floppy disk (in
general this corresponds to the root directory of the floppy). But
what we want is something that corresponds to the device special files
on Unix.

Unlike Unix, on Windows there are no special files as such, in the
sense that there would/could be such "special file inodes" on NTFS (or
FAT) volumes (pointed to by directory entries). But device names are
there anyway, they exist only in the kernel's namespace. Or something
like that. The SDK documentation from Microsoft for CreateFile() says
that the "file name" for the A: floppy drive device is \\.\A:.

With dd.exe from the findutils-4.1-1 package, running

dd if=\\.\A: of=disk.img bs=1k count=1440

will indeed write the whole content of disk A: into the file disk.img.
Also reading hard disks works fine, but here the name is
\\.\PHYSICALDRIVE<n>, for numerical values of n, like:

dd if=\\.\PHYSICALDRIVE0 of=drive0.img bs=1k count=100


dd.exe: More information why gnuwin32 fails
-------------------------------------------

Tor Lillqvist <tml@iki.fi> writes:

"For instance the following throwaway program succeeds in reading both
floppy and hard disk volumes. Yeah, the alignment logic is
unnecessarily wasteful of space".

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#define BUFFERSIZE (512*512)

int
main (int argc, char **argv)
{
  int f;
  char buf[2*BUFFERSIZE];
  char *aligned_bufp = (char *) ((((int) buf) + BUFFERSIZE-1) & (~(BUFFERSIZE-1)));
  int k;
  __int64 n;

  printf ("Reading %s\n", argv[1]); 

  if ((f = open (argv[1], O_RDONLY|O_BINARY)) == -1)
    perror ("open"), exit (1);

  n = 0;
  while ((k = read (f, aligned_bufp, BUFFERSIZE)) > 0)
    n += k;

  close (f);

  printf ("Got %I64d bytes\n", n);

  return 0;
}
