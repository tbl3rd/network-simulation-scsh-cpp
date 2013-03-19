#ifndef PROCESS_HH_INCLUDED
#define PROCESS_HH_INCLUDED


#include <assert.h>
#include <unistd.h>
#include <sys/wait.h>


// A handle on a process.
//
class Process
{
    int theId;

protected:

    // Fork the calling process leaving this a handle on the child process.
    //
    void fork() { theId = ::fork(); }

    // Map stdin, stdout, and stderr to in, out, and err respectively.
    //
    void mapIo(int in, int out, int err) {
        if (0 != in)  { ::close(0); ::dup(in);  }
        if (1 != out) { ::close(1); ::dup(out); }
        if (2 != err) { ::close(2); ::dup(err); }
    }

    // Overlay the calling program with a new image from path, and pass
    // argv to the result.
    //
    void exec(const char *path, const char *const *argv) {
        ::execvp((char *)path, (char **)argv);
        theId = -1;
    }

public:

    // Get a handle on the calling process.
    //
    Process(): theId(-1) { theId = ::getpid(); }

    // Fork a new process running path with argument vector argv.
    //
    Process(const char *path, const char *const *argv) {
        fork();
        if (0 == theId) exec(path, argv);
    }

    // Fork a new process running path with argument vector argv, and
    // stdin, stdout, and stderr set on in, out, and err respectively.
    //
    Process(
        const char *path, const char *const *argv,
        int in, int out = 1, int err = 2
    ) {
        fork();
        if (0 == theId) {
            mapIo(in, out, err);
            exec(path, argv);
        }
    }

    // Return non-0 if this process is OK.  Return 0 otherwise.
    //
    int ok() const { return theId != -1; }

    // Return the process ID of this.
    //
    int id() const { return theId; }

    // Wait for this process to exit.  Return the exit status.
    //
    int wait() {
        int status;
        int pid = ::waitpid(theId, &status, 0);
        assert(pid == theId);
        return status;
    }
};


#endif // ! PROCESS_HH_INCLUDE
