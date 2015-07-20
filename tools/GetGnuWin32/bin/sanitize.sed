#


#raysatiro@users.sourceforge.net
#written for getgnuwin32
#filename sanitizer sed script


#ignore lines with control characters (this includes tab (==d009))

/[\d000-\d031\d127]/d


#escape printable ascii characters other than 0-9A-Za-z
#because some characters like [ & ! ^ can cause parsing problems.
#printable extended ascii is not escaped


#  printf "\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x3a\x3b\x3c\x3d\x3e\x3f\x40\x5b\x5c\x5d\x5e\x5f\x60\x7b\x7c\x7d\x7e" | sed -f arf.out
#   !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
#  [ ][!]["][#][$][%][&]['][(][)][*][+][,][-][.]\/[:][;][<][=][>][?][@]\[\\\]\^[_][`][{][|][}][~]
#
# sed /

s/\\/\\\\/g
s/\//\\\//g
s/\]/\\\]/g
s/\[/\\\[/g
s/\^/\\^/g

s/[ ]/\[ \]/g
s/[!]/\[!\]/g
s/["]/\["\]/g
s/[#]/\[#\]/g
s/[$]/\[$\]/g
s/[%]/\[%\]/g
s/[&]/\[&\]/g
s/[']/\['\]/g
s/[(]/\[(\]/g
s/[)]/\[)\]/g
s/[*]/\[*\]/g
s/[+]/\[+\]/g
s/[,]/\[,\]/g
s/[-]/\[-\]/g
s/[.]/\[.\]/g
s/[:]/\[:\]/g
s/[;]/\[;\]/g
s/[<]/\[<\]/g
s/[=]/\[=\]/g
s/[>]/\[>\]/g
s/[?]/\[?\]/g
s/[@]/\[@\]/g
s/[_]/\[_\]/g
s/[`]/\[`\]/g
s/[{]/\[{\]/g
s/[|]/\[|\]/g
s/[}]/\[}\]/g
s/[~]/\[~\]/g



#now output our sed-as-grep commands to grep the filenames
#we're excluding filenames in exclude.txt from the download list
#s/^/\//
#s/$/\/d/
#s/^\(.*\)$/\/\1\/d/

