#include <assert.h>

#include <macaddress.hh>
#include <bridge_stub.hh>
#include <cap_bridge.hh>
#include <flow.hh>
#include <io.hh>

#include <algorithm>
#include <map>
#include <set>
#include <vector>


#define BEGINFORWARD OPEN "forward"
#define ENDFORWARD CLOSE
#define BEGINBRIDGE OPEN "bridge"
#define ENDBRIDGE CLOSE
#define BEGINLEARN OPEN "learn"
#define ENDLEARN CLOSE
#define BEGINMOVE OPEN "move"
#define ENDMOVE CLOSE
#define BEGINDELETE OPEN "delete"
#define ENDDELETE CLOSE


typedef vector<SaCapBridge *> Caps;
typedef set<SaFlow *, less<const SaFlow *> > Flows;
typedef pair<SaCapBridge *, Flows> CapFlows;
typedef map<MacAddress, CapFlows, less<MacAddress> > MacMap;
typedef MacMap::value_type MapEntry;


// Apply f to all items in the container c.
//
template<class Container, class Function>
static inline void forAll(Container &c, Function f)
{
    for_each(c.begin(), c.end(), f);
}


// A MapInsertResult is the result of a map<>::insert() call.  This is
// just a convenience class so I don't have to remember whether the pair
// is <boolean, iterator> or <iterator, boolean> ...
//
template<class Map>
struct MapInsertResult:
    public pair<typename Map::iterator, bool>
{
    typedef pair<typename Map::iterator, bool> Pair;
    const typename Map::iterator &iterator() { return first; }
    bool ok() const { return second; }
    MapInsertResult(const Pair &p): Pair(p) {}
};


// The following three definitions allow us to easily construct and
// deconstruct elements of a MacMap.
//
static inline const MacAddress &address(MapEntry &n) { return n.first; }
static inline SaCapBridge *&cap(MapEntry &n) { return n.second.first; }
static inline Flows &flows(MapEntry &n) { return n.second.second; }



// Cleaning up the bridge state consists of finding the MapEntry in the
// MacMap corresponding to the saved address and, if it still exists,
// removing the flow from the flow set.
//
class Deleter:
    public SaFlow::Deleter
{
    MacMap &itsMap;
    MacAddress addr;
public:
    virtual void operator()(const SaFlow *pFlow)
    {
        MacMap::iterator pEntry = itsMap.find(addr);
        if (pEntry != itsMap.end()) {
            SaFlow *const &rpFlow = pFlow; // help GCC sheesh!
            flows(*pEntry).erase(rpFlow);
            bool entryIsUseless = cap(*pEntry) && flows(*pEntry).empty();
            if (entryIsUseless) itsMap.erase(pEntry);
        }
    }
    Deleter(MacMap &map, const MacAddress &key): itsMap(map), addr(key) {}
};


// A function that registers a bridge with a bunch of caps.
// Application of this function effectively establishes a bridging
// domain over a set of leaf nodes (caps) in a Service Adaptation
// graph.
//
class Registrar
{
    SaBridge &itsBridge;
public:
    void operator()(SaCapBridge *p) { p->addUpper(itsBridge); }
    Registrar(SaBridge &bridge): itsBridge(bridge) {}
};

// Register this with all the caps in domain.
//
SaBridgeStub::SaBridgeStub(const Caps &domain):
    itsCaps(0), itsMacMap(0)
{
    itsCaps = new Caps(domain);
    itsMacMap = new MacMap;
    forAll(*itsCaps, Registrar(*this));
}


// A function that constructs an output cap list, by removing the
// input cap from a list of all caps in a domain.  In effect, we
// collect in itsOuts, the set difference (itsCaps - itsIn) when this
// is applied to the set of caps in itsCaps.
//
class Flooder
{
    Caps &itsOuts;
    SaCapBridge *const itsIn;
public:
    void operator()(SaCapBridge *p) { if (itsIn != p) itsOuts.push_back(p); }
    Flooder(SaCapBridge *pIn, Caps &outs): itsOuts(outs), itsIn(pIn) {}
};


// A hack to avoid dealing with `endstation frames' right now.
//
#ifdef NDEBUG
#error __FILE__:__LINE__ void SaBridgeStub::receive(SaFrame *) not implemented.
#else
void SaBridgeStub::receive(SaFrame *)
{
    // Just sink it for now.
}
#endif // NDEBUG


// A function that shows the cap at p on the stream s.
//
class ShowCap
{
    ostream &stream;
    const char *sep;
public:
    void operator()(const SaCapBridge *p)
    {
        assert(p);
        stream << sep << *p;
        sep = SPACE;
    }
    ShowCap(ostream &s): stream(s), sep("") {}
};


// Show a flow identified as a tagged pointer.
//
class ShowFlow
{
    ostream &stream;
    const char *sep;
public:
    void operator()(const SaFlow *p)
    {
        assert(p);
        stream << sep << *p;
        sep = SPACE;
    }
    ShowFlow(ostream &s): stream(s), sep("") {};
};


// Get an entry for addr from map.  Create one if necessary.
//
static inline MapEntry &getMapEntry(MacMap &map, const MacAddress &addr)
{
    MacMap::iterator pEntry = map.find(addr);
    if (pEntry == map.end()) {
        SaCapBridge *pNoCap = 0;
        MapEntry entry(addr, CapFlows(pNoCap, Flows()));
        MapInsertResult<MacMap> result = map.insert(entry);
        assert(result.ok());
        pEntry = result.iterator();
    }
    return *pEntry;
}

// Delete the flow at p after showing it.
//
struct ShowFlowThenDelete:
    public ShowFlow
{
    void operator()(const SaFlow *p)
    {
        ShowFlow::operator()(p);
        delete p;
    }
    ShowFlowThenDelete(ostream &s): ShowFlow(s) {}
};

// Make inCap the location for MAC address in entry.  Flush any old
// flows in the entry when the location changes.
//
static inline void rememberLocation(
    MacMap &map, const MacAddress &addr, SaCapBridge *pCap
)
{
    MapEntry &entry = getMapEntry(map, addr);

    if (cap(entry)) {
        cout << BEGINMOVE SPACE BEGINSTRING
             << address(entry) << ENDSTRING SPACE
             << *pCap << SPACE << *cap(entry) << ENDMOVE;
    } else {
        cout << BEGINLEARN SPACE BEGINSTRING
             << address(entry) << ENDSTRING SPACE
             << *pCap << ENDLEARN;
    }

    if (cap(entry) != pCap) {
        cap(entry) = pCap;
        cout << BEGINDELETE SPACE;
        forAll(flows(entry), ShowFlowThenDelete(cout));
        cout << ENDDELETE;
    }
}


// Add to outCaps all the caps to which a frame in the flow at pFlow,
// with destination MAC dstAddr and source MAC srcAddr and arriving on
// inCap, should be forwarded.
//
// First, augment the MacMap to remember that srcAddr was seen on
// inCap.  Then consult the MacMap to see whether the location of
// dstAddr is known.  If not, flood the flow.  If the location of
// dstAddr is known, then forward the flow to that cap only.
//
// Report and save the flow, and return true.
//
bool SaBridgeStub::forward(
    const MacAddress &dstAddr, const MacAddress &srcAddr,
    SaCapBridge *inCap, Caps &outCaps, SaFlow *pFlow
)
{
    rememberLocation(*itsMacMap, srcAddr, inCap);

    MapEntry &dstEntry = getMapEntry(*itsMacMap, dstAddr);

    if (cap(dstEntry)) {
        outCaps.push_back(cap(dstEntry));
    } else {
        forAll(*itsCaps, Flooder(inCap, outCaps));
    }

    cout << BEGINFORWARD SPACE BEGINSTRING << dstAddr
         << ENDSTRING SPACE BEGINSTRING << srcAddr
         << ENDSTRING SPACE << *inCap << SPACE OPEN;
    forAll(outCaps, ShowCap(cout));
    cout << CLOSE SPACE << pFlow << ENDFORWARD;

    MapInsertResult<Flows> result = flows(dstEntry).insert(pFlow);
    assert(result.ok());
    Deleter *pDeleter = new Deleter(*itsMacMap, dstAddr);
    assert(pDeleter);
    pFlow->push(pDeleter);

    return true;
}


// Show a MacMap map entry as list who's car is the string
// representation of the destination MAC address and who's
// cdr is the destination cap and a list of flows to that
// destination.
//
class ShowMapEntry
{
    ostream &stream;
    const char *sep;
public:
    void operator()(MapEntry &n)
    {
        stream << sep << OPEN BEGINSTRING << address(n) << ENDSTRING SPACE;
        if (cap(n)) {
            stream << *cap(n);
        } else {
            stream << "<destination unknown>";
        }
        stream << SPACE OPEN;
        forAll(flows(n), ShowFlow(stream));
        stream << CLOSE CLOSE;
        sep = SPACE;
    }
    ShowMapEntry(ostream &s): stream(s), sep("") {}
};


// Dump the flow table too.
//
SaBridgeStub::~SaBridgeStub()
{
    cout << endl << "Destructing bridge:" << endl;
    cout << BEGINBRIDGE SPACE OPEN;
    forAll(*itsCaps, ShowCap(cout));
    cout << CLOSE SPACE OPEN;
    forAll(*itsMacMap, ShowMapEntry(cout));
    cout << CLOSE ENDBRIDGE << endl;
    delete itsMacMap;
    delete itsCaps;
}
