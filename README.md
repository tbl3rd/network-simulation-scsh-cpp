network-simulation-scsh-cpp
===========================

The Scheme Shell (scsh) is a fantastically useful Unix shell embedded
in Scheme48 written by Olin Shivers and his henchmen.

    http://www.scsh.net/

This is an old sketch for a network simulator used to drive a C++
implementation of an IP service adaptation (read "dynamic embedded
virtual network") interface to a backbone telecommunications switch.

I began the project in Erlang, which I regretfully abandoned when the
idea started to gain value, because at the time Erlang still required
a license for commercial applications.  After the port to Scheme, the
project developed into a full-featured test bench for some hardware
and software I worked on.

And eventually the whole thing was rewritten in (what else?) C++.
