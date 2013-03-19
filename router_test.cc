
// Set up a simple SA graph topped by a stub implementation of an
// static IP router.  This program expects N (N > 0) integer
// command line arguments -- each value an open file descriptor.  Each
// open file descriptor represents a simulated LAN attachment, and a
// source of unrecognized frames to be forwarded.  Each unrecognized
// frame is passed up the SA protocol graph twice, which constructs an
// appropriate flow each time.  The resulting flows are compared for
// consistency.

// This program needs another program to open the file descriptors and
// feed simulated network data frames to the descriptors.  One such
// program is 'test-router.scm'.  Invoke 'test-router.scm' without
// arguments for a message on using it.  This program also prints a
// usage message when invoked without arguments.  Scheme scripts must
// executed in a bash shell


#include <memory.h>
#include <stdlib.h>
#include <strstream.h>

#include <algorithm>
#include <set>
#include <vector>

#include <ipaddress.hh>
#include <macaddress.hh>

#include <anchor_lan.hh>
#include <arp_enet_stub.hh>
#include <cap_bridge.hh>
#include <cap_router_ipv4.hh>
#include <fp_flow.hh>
#include <frame_sink.hh>
#include <if_translator.hh>
#include <node_csma_cd.hh>
#include <node_ipv4.hh>
#include <router_stub.hh>
#include <select.hh>
#include <test_util.hh>
#include <type.hh>
#include <unrecognized_frame.hh>

#include <io.hh>


// The SunOS headers don't declare these!  Can you believe that?-)
//
extern "C" int close(int);
extern "C" int getpid();
extern "C" void pause();
extern "C" int read(int, char *, int);
extern "C" long random();

typedef unsigned char Octet;
typedef SaRouterIPv4::Caps Caps;
typedef vector<SaAnchorLan *> Anchors;
typedef vector<MacAddress *> Macs;
typedef vector<SaNodeCsmaCd *> EnetNodes;

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


// Make one path from an anchor node to a bridge cap and router cap. 
// Set pAnchor to its anchor, pBridgeCap to the bridge cap and pRouterCap 
// to the router cap.  The caller adopts the anchor  and is responsible 
// for deleting it eventually either by calling delete or by destroying 
// the graph by calling SaNode::destroyGraph().  Return pAnchor.
//
// We use the file descriptor that models the point of attachment as
// the LAN ID for the associated AnchorLan object.  This hack works
// because LAN IDs and file descriptors are both just ints.
//
static SaAnchorLan *makeBridgeRouterTree(SaAnchorLan *&pAnchor, 
					 int fd, 
					 SaCapRouterIPv4 *&pRouterCap, 
					 SaCapBridge *&pBridgeCap) 
{
  static Sink sink;
  pAnchor = new SaAnchorLan(sink, fd);
  SaNodeCsmaCd *pCsmaCdNode = new SaNodeCsmaCd(*pAnchor);
  SaNodeIPv4 *pIPv4Node = new SaNodeIPv4(*pCsmaCdNode);
  pRouterCap = new SaCapRouterIPv4(*pIPv4Node);
  pBridgeCap = new SaCapBridge(*pCsmaCdNode);

  return pAnchor;
}

// Create a simple tree.  Return a string representation of the tree.
// Destroy the tree.
//
static char *dump()
{
    SaAnchorLan *pAnchor = 0;
    SaCapRouterIPv4 *pRouterCap = 0;
    SaCapBridge *pBridgeCap = 0;
    makeBridgeRouterTree(pAnchor, 0, pRouterCap, pBridgeCap);

    ostrstream s;
    pAnchor->dump(s);
    s << ends;
    cout << "dump(): " << endl << s.str() << endl;

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
    cout << "restore(): " << endl;
    pAnchor->dump(cout);
    cout << endl << endl;

    cout << SHOW(*pAnchor) << endl << endl;
    cout << SHOW(pAnchor->level()) << endl;
    cout << SHOW(pAnchor->degree()) << endl;
    cout << SHOW(pAnchor->root()) << endl;
    cout << SHOW(pAnchor->type()) << endl << endl;

    delete[] p;
    delete pNode;
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

// a predicate which puts random data in o
//
struct RandomizeOctet
{
  void operator()(Octet &o) { o = random() & 0xff; }
};


// Create the parameters for a default route, the default
// router's IP address, MAC address, and the segment to which 
// it is attached.
//
static void createDefaultRouter( const Caps caps, IpAddress &ip, 
				 MacAddress &mac, Caps &dCap )
{
  int c = random() % (caps.size()-1);
  dCap.push_back(caps[c]);

  RandomizeOctet randomize;
  forAll(ip, randomize);
  forAll(mac, randomize);
}

// Construct a vector of n random MacAddresses.
//
static void makeMacs( int n, Macs &m )
{
  RandomizeOctet randomize;
  for(int i=0; i<n; i++) { 
    MacAddress *mac = new MacAddress;
    forAll(*mac, randomize); 
    m.push_back(mac);
  }
}

// Construct a node graph stub for each MacAddress, each
// stub is rooted at an anchor returned in anchors and is
// fed frames by an fd. The translator t records the mappings
// of interfaces to SaNodeCsmaCds
//
static void makeStubs( const Macs &macs, int fds[], Anchors &a, 
		       SaIfTranslator &t)
{
  static Sink sink;

  for(unsigned int i=0; i<macs.size(); i++) { 
    SaAnchorLan *pAnchor = new SaAnchorLan(sink, fds[i]);
    SaNodeCsmaCd *pCsmaCdNode = new SaNodeCsmaCd(*pAnchor);
    pCsmaCdNode->addMac(*macs[i]);
    a.push_back(pAnchor);
    t.record(i, pCsmaCdNode);
  }
}


// Construct an appropriate Service Adaptation graph over the n points
// of attachment represented in the array of file descriptors, fds.
//
static bool testRouter(int n, int fds[])
{
    // simple node graph check
    restore(dump());

    bool result = true;

    {                           // blocks to order ctors and dtors
        Macs macs;
        makeMacs(n, macs);
	Anchors anchors;
	SaIfTranslator t;
	makeStubs(macs, fds, anchors, t);

	SaRouterStub router(t);
	SaArpEnetStub arp;

	Caps caps;
	for(int i=0; i<n; i++) {
	  caps.push_back(router.addIf(i, arp));
	}

	// create a default router and install a default route 
	// in router
	//
	IpAddress dIp;
	MacAddress dMac;
	Caps dCap;

	createDefaultRouter(caps, dIp, dMac, dCap);

	router.addRoute(dIp, dCap);
	arp.addMap(dIp, dMac);

        {
            FlowTracker tracker;
            FrameReader reader(n, fds);

            cout << OPEN "router-events";

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

            cout << "Number of flows that survive test: "
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


// Pause the program pending a signal if the SA_DEBUG_BRIDGE_TEST
// environment variable is set.  You can `attach PID' a debugger to
// the process while it is blocked in pause(), set breakpoints and so
// on, then `c'ontinue the program.  If this is part of a
// multithreaded program, you may need to establish a signal handler
// to avoid killing the process, because the GDB debugger uses the
// kill(0) hack, which is not standard Posix.
//
static void pauseIfDebugging()
{
    if (getenv("SA_DEBUG_ROUTER_TEST")) {
        ostrstream s;
        s << "\n\nProcess ID: (gdb) attach " << getpid() << "\n\n" << ends;
        char *msg = s.str();
        cerr << msg;
        delete[] msg;
        pause();
    }
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

        int poaFds[poaCount];
        static const int bogusFdValue = -1;

        poaFds[poaCount] = bogusFdValue;
        for (int n = 1; ok && n <= poaCount; ++n) {
            istrstream s(av[n]);
            ok = s >> poaFds[n - 1];
        }
        ok = ok && poaFds[poaCount] == bogusFdValue;

        ok = ok && testRouter(poaCount, poaFds);
    }

    return !ok;
}

