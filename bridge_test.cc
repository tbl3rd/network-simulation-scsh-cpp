
// Set up a simple SA graph topped by a stub implementation of an
// unmanaged learning bridge.  This program expects N (N > 0) integer
// command line arguments -- each value an open file descriptor.  Each
// open file descriptor represents a simulated LAN attachment, and a
// source of unrecognized frames to be forwarded.  Each unrecognized
// frame is passed up the SA protocol graph twice, which constructs an
// appropriate flow each time.  The resulting flows are compared for
// consistency.

// This program needs another program to open the file descriptors and
// feed simulated network data frames to the descriptors.  One such
// program is 'test-bridge.scm'.  Invoke 'test-bridge.scm' without
// arguments for a message on using it.  This program also prints a
// usage message when invoked without arguments.


#include <memory.h>
#include <stdlib.h>
#include <strstream.h>

#include <algorithm>
#include <set>
#include <vector>

#include <macaddress.hh>

#include <anchor_lan.hh>
#include <bridge_stub.hh>
#include <cap_bridge.hh>
#include <fp_flow.hh>
#include <frame_sink.hh>
#include <node_csma_cd.hh>
#include <select.hh>
#include <type.hh>
#include <unrecognized_frame.hh>

#include <pause.hh>
#include <test_util.hh>
#include <io.hh>


// The SunOS headers don't declare these!  Can you believe that?-)
//
extern "C" int close(int);
extern "C" int read(int, char *, int);


typedef unsigned char Octet;
typedef SaBridge::Caps Caps;
typedef vector<SaAnchorLan *> Anchors;


// Apply f to all items in the container c.
//
template<class Container, class Function>
static inline void forAll(Container &c, Function f)
{
    for_each(c.begin(), c.end(), f);
}


// A stub frame sink class.
//
struct Sink: SaFrameSink { void send(SaFrame *) {} };


// Make one path from an anchor node to a bridge cap serving the
// protocol designated by P.  Set pAnchor to its anchor and pCap to
// its cap.  The caller adopts the anchor and is responsible for
// deleting it eventually either by calling delete or by destroying
// the graph by calling SaNode::destroyGraph().  Return pAnchor.
//
// We use the file descriptor that models the point of attachment as
// the LAN ID for the associated AnchorLan object.  This hack works
// because LAN IDs and file descriptors are both just ints.
//
static SaAnchorLan *makeBridgeTree(SaAnchorLan *&pAnchor, int fd, 
				   SaCapBridge *&pCap) 
{
  static Sink sink;
  pAnchor = new SaAnchorLan(sink, fd);
  SaNodeCsmaCd *pCsmaCdNode = new SaNodeCsmaCd(*pAnchor);
  pCap = new SaCapBridge(*pCsmaCdNode);
  return pAnchor;
}

// Create a simple tree.  Return a string representation of the tree.
// Destroy the tree.
//
static char *dump()
{
    SaAnchorLan *pAnchor = 0;
    SaCapBridge *pCap = 0;
    makeBridgeTree(pAnchor, 0, pCap);

    ostrstream s;
    pAnchor->dump(s);
    s << ends;
    cout << endl << s.str() << endl;

    delete pAnchor;
    return s.str();
}


// Restore a simple tree from its string representation at p.  Dump
// some data about the tree.  Delete p and the tree before returning.
//
static void restore(char *p)
{
    istrstream s(p);
    SaNode *pNode = SaNode::restore(s);
    SaAnchor *pAnchor = SA_DYNAMIC_CAST(SaNode, SaAnchor, pNode);
    assert(pAnchor);

    cout << endl;
    pAnchor->dump(cout);
    cout << endl << endl;

    cout << SHOW(*pAnchor) << endl << endl;
    cout << SHOW(pAnchor->level()) << endl;
    cout << SHOW(pAnchor->degree()) << endl;
    cout << SHOW(pAnchor->root()) << endl;
    cout << SHOW(pAnchor->type()) << endl;

    delete[] p;
    delete pNode;
}


// Make a graph of n bridge trees.
// Store the anchors in a and the caps in c.
//
static void makeBridgeGraph(int n, Anchors &a, int fds[], Caps &c)
{
    while (n--) {
        SaAnchorLan *pAnchor = 0;
        SaCapBridge *pCap = 0;
        makeBridgeTree(pAnchor, fds[n], pCap);
        a.push_back(pAnchor);
        c.push_back(pCap);
    }

    cout << endl << OPEN;
    DumpId dumpIt(cout);
    SaNode::find(dumpIt);
    cout << CLOSE << endl << endl;
}


class FlowTracker;

// Tell a FlowTracker, itsTracker, to forget the flow at p.
//
class FlowTrackerDeleter:
    public SaFlow::Deleter
{
    FlowTracker &itsTracker;
public:
    void operator()(const SaFlow *p);
    FlowTrackerDeleter(FlowTracker &t): itsTracker(t) {}
};

static void deleteFlow(SaFlow *p) { delete p; }

// Track flows on the heap so they can be eventually freed.  Any flow
// produced by a FlowTracker is deleted when the FlowTracker is
// deleted, unless the FlowTracker was told to forget() it.
//
class FlowTracker
{
    typedef set<SaFlow *, less<const SaFlow *> > Flows;
    Flows itsFlows;

public:

    // Allocate a new flow and remember it for deletion when this is
    // destructed.  Return the new flow.
    //
    FpFlowBase *remember()
    {
        FpFlowBase *pF = new FpFlowBase;
        assert(pF);
        FlowTrackerDeleter *pD = new FlowTrackerDeleter(*this);
        assert(pD);
        pF->push(pD);
        itsFlows.insert(pF);
        return pF;
    }

    // Forget the flow at p.  Don't delete it when this is destructed.
    //
    void forget(const SaFlow *p)
    {
        SaFlow *nonconstP = const_cast<SaFlow *>(p);
        itsFlows.erase(nonconstP);
    }

    // Return the number of flows that this remembers.
    //
    int size() const { return itsFlows.size(); }

    // Delete all flows that have not been forgotten.
    //
    ~FlowTracker() { forAll(itsFlows, deleteFlow); }
};

void FlowTrackerDeleter::operator()(const SaFlow *p)
{
    itsTracker.forget(p);
}


// Get a frame.
//
class FrameReader
{
    // This must be greater than the size of any incoming frame.
    //
    enum { bufferSize = 2048 };

    FileDescriptorSet itsOpenSet;
    FileDescriptorSet itsPendingSet;
    FileDescriptorSet::iterator itsNext;
    int itsFd;

    typedef int CountType;
    typedef FileDescriptorSet::value_type FileDescriptor;

    union {
        double justForAlignment;
        CountType octetCount;
        Octet buffer[bufferSize];
    } u;

    // Determine which file descriptors in itsOpenSet have frame input
    // waiting to be read.  Store the result in itsPendingSet, and
    // reset itsNext.
    //
    void more()
    {
        if (itsNext == itsPendingSet.end()) {
            itsPendingSet = itsOpenSet.selectRead();
            assert(itsPendingSet);
            itsNext = itsPendingSet.begin();
        }
    }

    // Attempt to read n octets from the file descriptor at itsNext.
    // Return the number of octets actually read.
    //
    CountType readOctets(CountType n)
    {
        static const int badRead = -1;
        assert(n > 0);
        assert(n < bufferSize);
        int result = read(*itsNext, reinterpret_cast<char *>(u.buffer), n);
        assert(result != badRead);
        assert(result < bufferSize);
        return result;
    }

    // Close the file descriptor at itsNext.
    //
    void closeFd()
    {
        FileDescriptor fd = *itsNext;
        itsOpenSet.clear(fd);
        itsPendingSet.clear(fd);
        ::close(fd);
    }

    // Attempt to read a message from the next pending file descriptor.
    // Return true unless there is an end-of-file condition.  In that
    // case, return false, and close the exhausted file descriptor.
    //
    bool readMessage()
    {
        more();
        bool gotEof = 0 == readOctets(sizeof(u.octetCount));
        if (gotEof) {
            closeFd();
        } else {
            CountType messageSize = u.octetCount;
            CountType count = readOctets(messageSize);
            assert(count == messageSize);
            itsFd = *itsNext;
        }
        ++itsNext;
        return !gotEof;
    }

public:

    // Attempt to get the next frame.  Return true if there is a next
    // frame.  Return false if there are no more frames.
    //
    bool next()
    {
        bool ok = itsOpenSet;
        while (ok && !readMessage()) ok = itsOpenSet;
        return ok;
    }

    // Return the file descriptor on which the frame at begin() arrived.
    //
    int fd() const { return itsFd; }

    // Return a pointer to the first octet of the current frame.
    //
    Octet *begin() { return u.buffer; }

    // Return a pointer past the last octet of the current frame.
    //
    const Octet *end() const { return u.buffer + sizeof(u.buffer); }

    FrameReader(int fdCount, int fds[]):
        itsOpenSet(fdCount, fds),
        itsPendingSet(),
        itsNext(itsPendingSet.end()),
        itsFd(-1)
    {}
};


// Construct an appropriate Service Adaptation graph over the n points
// of attachment represented in the array of file descriptors, fds.
//
static bool testBridge(int n, int fds[])
{
    bool result = true;

    {                           // blocks to order ctors and dtors
        Anchors anchors;
        Caps caps;
        makeBridgeGraph(n, anchors, fds, caps);
        SaBridgeStub bridge(caps);

        {
            FlowTracker tracker;
            FrameReader reader(n, fds);

            cout << OPEN "bridge-events";

            while (result && reader.next()) {
                SaUnrecognizedFrame frame(reader.begin(), reader.end());
                FpFlowBase *pFlow1 = tracker.remember();
                FpFlowBase *pFlow2 = tracker.remember();
                cout << SPACE;
                result = SaAnchorLan::find(reader.fd())->flowIn(frame, pFlow1);
                result = result
                    && SaAnchorLan::find(reader.fd())->flowIn(frame, pFlow2);
                result = result && *pFlow1 == *pFlow2;
            }

            cout << CLOSE << endl << endl;

            cout << "Number of flows that survive learning: "
                 << SHOW(tracker.size()) << endl;
        }
    }

    SaNode::destroyGraph();     // Must call this after destructors.

    return result;
}


static void showArg(const char *p) { cerr << ' ' << p; }


// Return the number of command line arguments.  Print a usage message
// to stderr if there are not enough, or too many.
//
int countFileDescriptors(int ac, char *av[])
{
    static const int maxArgs = FileDescriptorSet::max_size();

    bool ok = ac > 1 && ac < maxArgs;

    if (!ok) {
        cerr << "Usage: " << av[0] << " fd [fd, ...]" << endl;
        cerr << endl;
        cerr << "Where: fd is an integer representing an open" << endl;
        cerr << "       file descriptor for the process which" << endl;
        cerr << "       execs this program." << endl;
        cerr << endl;
        cerr << "Example: " << av[0] << " 5 6 7 8 9 10 11 12 13" << endl;
        cerr << endl;
        cerr << "Invoked:"; for_each(av, av + ac, showArg); cout << endl;
    }

    return ok? ac - 1: 0;
}


// Parse file descriptors representing LAN segment points of
// attachment off the command line and pass them to test code.
//
int main(int ac, char *av[])
{
    pauseIfDebugging();

    static const int poaCount = countFileDescriptors(ac, av);
    bool ok = poaCount > 0;

    if (ok) {

        restore(dump());

        int poaFds[poaCount];
        static const int bogusFdValue = -1;

        poaFds[poaCount] = bogusFdValue;
        for (int n = 1; ok && n <= poaCount; ++n) {
            istrstream s(av[n]);
            ok = s >> poaFds[n - 1];
        }
        ok = ok && poaFds[poaCount] == bogusFdValue;

        ok = ok && testBridge(poaCount, poaFds);
    }

    return !ok;
}
