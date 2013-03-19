#ifndef PAUSE_HH_INCLUDED
#define PAUSE_HH_INCLUDED


#include <iostream.h>


// The SunOS headers don't declare these!  Can you believe that?-)
//
extern "C" int getpid();
extern "C" int pause();


// Pause the program pending a signal if the SA_DEBUG_TEST environment
// variable is set.  You can `attach PID' a debugger to the process
// while it is blocked in pause(), set breakpoints and so on, then
// `c'ontinue the program.  If this is part of a multithreaded
// program, you may need to establish a signal handler to avoid
// killing the process, because the GDB debugger uses the kill(0)
// hack, which is not standard Posix.
//
static void pauseIfDebugging()
{
    if (getenv("SA_DEBUG_TEST")) {
        ostrstream s;
        s << "\n\nProcess ID: (gdb) attach " << getpid() << "\n\n" << ends;
        char *msg = s.str();
        cerr << msg;
        delete[] msg;
        pause();
    }
}


#endif // ! PAUSE_HH_INCLUDED
