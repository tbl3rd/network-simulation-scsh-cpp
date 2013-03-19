#include <errno.h>
#include <signal.h>
#include <string.h>

#include <iostream.h>
#include <strstream.h>

#include <bits.hh>
#include <bits.cc>

#include <message.hh>
#include <process.hh>
#include <socket.hh>

#include <pause.hh>


// Test UDP and TCP sockets by squirting CD liner notes from and to
// them.  Each title is sent in a variably-sized message preceeded by
// a fixed-size message that tells the receiver how many bytes to
// expect in the next message.  In other words, the test assumes a
// reliable transport.

// The test uses three processes: a receiver process and two sender
// processes.  The receiver process starts first and spawns a sender
// process named SENDNAME for each protocol in succession.  The test
// is successful if all the title messages are received.  The test
// uses function templates and specialization to test both protocols
// without duplicating very much of the code.

// For example, we rely on datagram chunking and eliminate the sending
// of size messages between title messages when testing UDP sockets.
// Consequently, sendSizeMessage() and receiveSizeMessage() don't
// actually send or receive messages when called on UDP sockets, but
// they do when called on TCP sockets.


#define SENDNAME "sender"
#define SHOW(X) #X " == " << X
#define BOOLEAN(B) ((B)? "true": "false")
#define NOTOR(B) ((B)? "": "not ")
#define OPEN "("
#define CLOSE ")"
#define SPACE " "
#define BEGIN "\""
#define END BEGIN


// These are for printing debug traces.
//
extern "C" int getpid();
extern "C" const char *sys_errlist[];


// The type of values of sizeof expressions and pointer differences.
// ANSI says these should be size_t and ptrdiff_t, but that seems too
// fussy -- even for me.
//
typedef int Count;


// Dump socket state.
//
ostream &operator<<(ostream &o, const SaSocketIpUdp &s)
{
    return o << OPEN "UDP" SPACE << s.fileDescriptor() << SPACE BEGIN
             << s.receiveAddress() << END SPACE BEGIN
             << s.sendAddress() << END SPACE OPEN
             << NOTOR(s) << "ok" CLOSE SPACE OPEN
             << NOTOR(s.isConnected()) << "connected" CLOSE CLOSE;
}
ostream &operator<<(ostream &o, const SaSocketIpTcp &s)
{
    return o << OPEN "TCP" SPACE << s.fileDescriptor() << SPACE BEGIN
             << s.receiveAddress() << END SPACE BEGIN
             << s.sendAddress() << END SPACE OPEN
             << NOTOR(s) << "ok" CLOSE SPACE OPEN
             << NOTOR(s.isConnected()) << "connected" CLOSE SPACE OPEN
             << NOTOR(s.itsListening) << "listening" CLOSE CLOSE;
}



// A 4-octet message conveying a count.
//
class SizeMessage:
    public SaMessage
{
    union {
        Count itsCount;
        Octet itsRep[sizeof(Count)];
    };

public:

    void find(Predicate &p) { p(itsRep, itsRep + sizeof(itsRep)); }

    Count count() const { return itsCount; }

    SizeMessage(Count n): itsCount(n) {}
    SizeMessage(): itsCount(-1) {}
};


// A message of variable size conveying a null-terminated C string.
//
class TestMessage:
    public SaMessage
{
    Count itsIndex;
    const char *const itsBeginP;
    const char *itsEndP;

public:

    friend ostream &operator<<(ostream &s, const TestMessage &m)
    {
        return s << '[' << m.itsIndex << "] == " << m.itsBeginP;
    }

    void find(Predicate &p)
    {
        bool stop = p(
            static_cast<Octet *>(&itsIndex),
            static_cast<Octet *>(&itsIndex + 1)
        );
        if (!stop) p(
            static_cast<Octet *>(itsBeginP), static_cast<Octet *>(itsEndP)
        );
    }

    Count count() const { return itsEndP - itsBeginP; }

    explicit TestMessage(Count n, const char *s):
        itsIndex(n), itsBeginP(s), itsEndP(itsBeginP)
    {
        while (*itsEndP++) continue;
    }
};


// A buffer capable of receiving a message of variable size.
//
class TestMessageBuffer:
    public SaMessage
{
    enum { itsAllocatedSize = 1024 };
    Count itsIndex;
    Octet itsBuffer[itsAllocatedSize];
    Octet *itsEndP;
    bool itsOk;

public:

    friend ostream &operator<<(ostream &s, const TestMessageBuffer &m)
    {
        return s << '[' << m.itsIndex << "] == " << m.itsBuffer;
    }

    bool ok() const { return itsOk; }

    Count index() const { return itsIndex; }

    void find(Predicate &p)
    {
        itsOk = !p(
            static_cast<Octet *>(&itsIndex),
            static_cast<Octet *>(&itsIndex + 1)
        );
        if (itsOk) p(itsBuffer, itsEndP);
    }

    static Count allocatedSize() { return itsAllocatedSize; }

    TestMessageBuffer():
        itsEndP(itsBuffer + itsAllocatedSize), itsOk(false)
    {
        itsBuffer[0] = '\0';
    }

    TestMessageBuffer(Count n):
        itsEndP(itsBuffer + n), itsOk(false)
    {
        assert(n <= itsAllocatedSize);
        itsBuffer[0] = '\0';
    }
};


// An array of liner notes (song titles) from Eno's classic ...
//
static const char *const brianEno[] = {
    "\"Before and after Science\"",
    "\"No one receiving\"",
    "\"Backwater\"",
    "\"Kurt's Rejoinder\"",
    "\"Energy fools the Magician\"",
    "\"King's Lead Hat\"",
    "\"Here he comes\"",
    "\"Julie with ...\"",
    "\"By this River\"",
    "\"Through Hollow Lands (for Harold Budd)\"",
    "\"Spider and I\""
};

#define ARRAYCOUNT(A) (sizeof(A) / sizeof(A[0]))


// A class to initialize an array of TestMessage objects.
// TestMessageInitializer effectively thunkifies the TestMessage
// constructor so it can be called from new[] expressions.
//
class TestMessageInitializer:
    public TestMessage
{
    static const char *const *brianEno() { return ::brianEno; }

    static Count songIndex;

public:

    static Count count() { return ARRAYCOUNT(::brianEno); }

    TestMessageInitializer():
        TestMessage(songIndex, brianEno()[songIndex])
    {
        ++songIndex;
    }
};

Count TestMessageInitializer::songIndex = 0;


// Return the string representation of the address of the socket s.
// Caller must delete[] the returned pointer.
//
static char *socketReceiveAddressString(SaSocketIp &s)
{
    assert(s);
    ostrstream os;
    os << s.receiveAddress() << ends;
    return os? os.str(): 0;
}


// Return the number of elements in the null-terminated vector v.
//
template<class T>
static inline Count sizeofNtv(const T *const *v)
{
    const T *const *p = v;
    while (*p) ++p;
    return 1 + p - v;
}

// Concatenate the C argument vectors prefix and suffix into a single
// argument vector comprising the elements of prefix followed by the
// arguments of suffix.  The caller must delete the returned pointer.
//
static const char **catArgVectors(
    const char **prefix, const char **suffix
)
{
    assert(prefix && suffix);
    const char **const av
        = new const char *[sizeofNtv(prefix) + sizeofNtv(suffix)];

    if (av) {
        const char **p = av;
        while ((*p = *prefix++)) ++p;
        while ((*p = *suffix++)) ++p;
    }

    return av;
}


// Receive a message size message if necessary.  If s is a datagram
// socket, use some static maximum size instead of receiving a count
// message.  If s is a stream socket, receive a size message to chunk
// the data coming off the stream into messages.
//
template<class Socket>
Count receiveSizeMessage(Socket &s)
{
    return TestMessageBuffer::allocatedSize();
}
Count receiveSizeMessage(SaSocketIpTcp &s)
{
    SizeMessage size;
    s.receive(size);
    return s? size.count(): -1;
}

// Receive a pre-defined set of messages on a socket s.
//
template<class Socket>
bool receiveMessages(Socket &s)
{
    static unsigned messageCount = TestMessageInitializer::count();
    Bits<32> seen;
    assert(messageCount <= seen.size());

    static const Bits<32> done((1UL << messageCount) - 1UL);

    while (s && seen != done) {
        Count sizeCount = receiveSizeMessage(s);
        if (s) {
            TestMessageBuffer message(sizeCount);
            s.receive(message);
            if (message.ok()) {
                seen[message.index()] = 1;
                cout << "Receive: message" << message << endl;
            } else {
                cout << "Received junk!" << endl;
            }
        }
    }

    return s && seen == done;
}


// Call accept() on a socket `from' returning a potentially new socket
// `to'.  If the socket does not support a connection-oriented
// protocol, simply copy the `from' socket onto the `to' socket.
//
template<class Socket>
bool accept(Socket &from, Socket &to)
{
    return to = from;
}
bool accept(SaSocketIpTcp &from, SaSocketIpTcp &to)
{
    return to = from.accept();
}

// Open a receive socket, fork a send process and wait for connections
// and messages.
//
template<class Socket>
int receive(int, char *av[], const char *tag, Socket *)
{
    Socket local;

    bool ok = local;

    if (ok) {
        cout << av[0] << ": " << SHOW(local) << endl;

        const char *sendPrefix[4];
        sendPrefix[0] = SENDNAME;
        sendPrefix[1] = socketReceiveAddressString(local);
        sendPrefix[2] = tag;
        sendPrefix[3] = 0;

        const char **const sendAv
            = catArgVectors(sendPrefix, static_cast<const char **>(&av[1]));

        Process sender(av[0], sendAv);

        Socket remote;
        ok = ok && accept(local, remote);

        cout << SHOW(errno) << " " SHOW(sys_errlist[errno]) << endl;
        cout << av[0] << ": " << SHOW(local) << endl;
        cout << av[0] << ": " << SHOW(remote) << endl;

        ok = ok && sender.ok();
        ok = ok && receiveMessages(remote);
        ok = ok && 0 == sender.wait();

        delete[] sendAv;
        delete[] sendPrefix[1];
    }

    return !ok;
}


// Send a message size message if necessary.  If s is a datagram
// socket, don't bother sending anything, and just rely on the
// chunking inherent in datagram messages.  (This assumes no
// fragmentation in the transport.)  If s is a stream socket, send a
// size message to help the receiver chunk the data coming off the
// stream back into delimited messages.
//
template<class Socket>
void sendSizeMessage(Socket &, TestMessage &) {}
void sendSizeMessage(SaSocketIpTcp &s, TestMessage &m)
{
    SizeMessage size(m.count());
    s.send(size);
}

// Send messages on s.
//
template<class Socket>
bool sendMessages(Socket &s)
{
    const Count count = TestMessageInitializer::count();
    TestMessage *message = new TestMessageInitializer[count];
    for (int n = 0; s && n < count; ++n) {
        cout << SENDNAME ": message" << message[n] << endl;
        sendSizeMessage(s, message[n]);
        if (s) s.send(message[n]);
    }

    cout << SENDNAME ": " SHOW(errno) << " " SHOW(sys_errlist[errno]) << endl;
    cout << SENDNAME ": " << s << endl;

    delete[] message;
    return s;
}


// Open a connected socket, and send messages on it.
//
template<class Socket>
int send(int, char *av[], Socket *)
{
    istrstream is(av[1]);
    SaAddressIpSocket address;
    is >> address;
    Socket socket(address);

    cout << av[0] << ": " SHOW(errno) << " " SHOW(sys_errlist[errno]) << endl;
    cout << av[0] << ": " << socket << endl;

    bool ok = sendMessages(socket);
    return !ok;
}


// Return true if the null-terminated C strings at lhs and rhs are
// equal character for character.  Return false if they differ.
//
inline bool stringEqual(const char *lhs, const char *rhs)
{
    return !strcmp(lhs, rhs);
}


// Return 0 if tests complete successfully.  Return something else if
// the tests detect a failure somewhere.  Pass a final tag string
// argument to the sender process so it knows what type of socket to
// open on the address passed to it.
//
int main(int ac, char *av[])
{
    errno = 0;
    pauseIfDebugging();
    signal(SIGPIPE, SIG_IGN);

    cout << "Process " << getpid() << " in main(): ";
    for (int n = 0; n < ac; ++n) cout << av[n] << ' ';
    cout << endl;

    static SaSocketIpUdp *const pUdp = 0;
    static SaSocketIpTcp *const pTcp = 0;
    static const char tagUdp[] = "UDP";
    static const char tagTcp[] = "TCP";

    int result = 0;

    if (stringEqual(av[0], SENDNAME)) {
        if (stringEqual(av[2], tagUdp)) result = send(ac, av, pUdp);
        if (stringEqual(av[2], tagTcp)) result = send(ac, av, pTcp);
    } else {
        result |= receive(ac, av, tagTcp, pTcp);
        result |= receive(ac, av, tagUdp, pUdp);
    }

    return result;
}
