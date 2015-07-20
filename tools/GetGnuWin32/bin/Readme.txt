-------------------------------------------------------------------

Maintaining a GnuWin32 package list and installation directory

author : Jay Satiro <raysatiro@users.sourceforge.net>
         Mathias Michaelis <michaelis@tcnet.ch>
date   : November 15, 2009
version: 0.6.3

Copyright (c) 2009 by Jay Satiro <raysatiro@users.sourceforge.net>

-------------------------------------------------------------------

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.2
or any later version published by the Free Software Foundation;
with no Invariant Sections, no Front-Cover Texts, and no Back-Cover
Texts.  A copy of the license is included in the file LICENSE.TXT


Where are the files from this directory are from?
-------------------------------------------------

Most of the .dll and .exe files in this directory comes from an old
package of the gnuwin32 project. sort.exe was compiled however by
Jay Satiro <raysatiro@users.sourceforge.net>. Here some remarks on
some of the programs used here:


sort.exe
--------

sort.exe was compiled by Jay Satiro <raysatiro@users.sourceforge.net>
from the source file sort.c from the coreutils-7.6 package. This was
necessary because sort from coreutils-5.3 (currently the most recent
package available from gnuwin32) has some serious problems. Most
notably it apparently does not honor LC_ALL. What this means is that
if your Windows has Danish locale for example, sort 5.3 will not sort
as expected when LC_ALL=C.

C:\Users\Internet\Desktop\sort>cat amp3
bb
aa
ae
bb

C:\Users\Internet\Desktop\sort>set LC_ALL=Danish

C:\Users\Internet\Desktop\sort>sort-7.6 amp3
ae
bb
bb
aa

C:\Users\Internet\Desktop\sort>set LC_ALL=C

C:\Users\Internet\Desktop\sort>sort-7.6 amp3
aa
ae
bb
bb

Also 7.6 has version sorting and operates much faster.

Note that the compiled version in this director is a test build. You
are welcome to use it, but do not replace your current sort. Suggested
rename as sort-7.6.exe before copying it to gnuwin32\bin.


sed.exe
-------

The included build of sed is now version 4.2. It is the same build released by gnuwin32.
Sed 4.1.5 has some problems with byte ranges:

C:\Users\Internet\Desktop>printf "a" | sed-4.1.5 -n "/[^\d032-\d126]/p"
a
C:\Users\Internet\Desktop>printf "a" | sed-4.1.5 -n "/[\d032-\d126]/p"

C:\Users\Internet\Desktop>printf "a" | sed-4.2 -n "/[^\d032-\d126]/p"

C:\Users\Internet\Desktop>printf "a" | sed-4.2 -n "/[\d032-\d126]/p"
a
C:\Users\Internet\Desktop>printf "~" | sed-4.1.5 -n "/[^\d032-\d126]/p"

C:\Users\Internet\Desktop>printf "~" | sed-4.1.5 -n "/[\d032-\d126]/p"
~
C:\Users\Internet\Desktop>printf "~" | sed-4.2 -n "/[^\d032-\d126]/p"

C:\Users\Internet\Desktop>printf "~" | sed-4.2 -n "/[\d032-\d126]/p"
~
