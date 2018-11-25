Commentary:

Two alternative versions of telnet functions for MS Windows.

The first, called `telnet' here, uses a Windows version of telnet
by Igor Milavec that was modified to use stdio by Naftali Ramati
<naftali@harmonic.co.il> (includes source).  Unfortunately, this
telnet program requires that the host be specified on the command
line, but standard `telnet-mode' wants to specify the host using
the "open" command once telnet has started. Zoltan Kemenczy
<zoltan@nabu.isg.mot.com> wrote this version of `telnet-mode',
which does this.

The second, called `telnet-sailor' here, was written by Ngai Kim
Hoong <wsailor@hotpop.com> for use with Cygwin. (This one doesn't
hide your password.)
