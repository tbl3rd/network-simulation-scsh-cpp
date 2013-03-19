#include <assert.h>

#include <arp_enet.hh>
#include <cap_router_ipv4.hh>
#include <flow.hh>
#include <if_translator.hh>
#include <ipaddress.hh>
#include <io.hh>
#include <node_csma_cd.hh>
#include <router_stub.hh>

#include <algorithm>
#include <map>
#include <set>
#include <vector>

#define BEGINFORWARD OPEN "forward"
#define ENDFORWARD CLOSE
#define BEGINROUTER OPEN "router"
#define ENDROUTER CLOSE
#define BEGINUNREACH OPEN "no route"
#define ENDUNREACH CLOSE

typedef vector<SaCapRouterIPv4 *> Caps;

// for a map with entries which look like:
//
// IpAddress (key) -> IpAddress, Cap, Flows
//
typedef set<SaFlow *, less<const SaFlow *> > Flows;
typedef pair<SaCapRouterIPv4 *, Flows> CapFlows;
typedef pair<IpAddress, CapFlows> AddrCapFlows;
typedef map<IpAddress, AddrCapFlows, less<IpAddress> > IpAddrMap;
typedef IpAddrMap::value_type IpAddrEntry;


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


// The following definitions allow us to easily construct and deconstruct 
// elements of a IpAddrMap.
//
static inline const IpAddress &keyAddr(IpAddrEntry &n)
{ 
  return n.first; 
}

static inline const IpAddress &nextHopAddr(IpAddrEntry &n) 
{ 
  return n.second.first; 
}

static inline SaCapRouterIPv4 *&cap(IpAddrEntry &n) 
{ 
  return n.second.second.first; 
}

static inline Flows &ipFlows(IpAddrEntry &n) 
{ 
  return n.second.second.second; 
}


// Cleaning up the router state consists of finding the IpAddrEntry in the
// IpAddrMap corresponding to the saved address and, if it still exists,
// removing the flow from the flow set.  
//
class Deleter:
  public SaFlow::Deleter
{
  IpAddrMap &itsIpAddrMap;
  IpAddress addr;
  
public:
  virtual void operator()(const SaFlow *pFlow)
  {
    IpAddrMap::iterator pEntry = itsIpAddrMap.find(addr);
    
    if (pEntry != itsIpAddrMap.end()) {
      SaFlow *const &rpFlow = pFlow; // help GCC sheesh!
      ipFlows(*pEntry).erase(rpFlow);
    }
  }

  Deleter(IpAddrMap &ipAddr, const IpAddress &key): 
    itsIpAddrMap(ipAddr), addr(key) {}
};


// Create a router w/o any caps
//
SaRouterStub::SaRouterStub(const SaIfTranslator &t):
    itsCaps(0), itsIpAddrMap(0), itsIfTranslator(&t)
{
  itsCaps = new Caps;
  itsIpAddrMap = new IpAddrMap;
}

// A hack to avoid dealing with `endstation frames' right now.
//
#ifdef NDEBUG
#error __FILE__:__LINE__ void SaRouterStub::receive(SaFrame *) not implemented.
#else
void SaRouterStub::receive(SaFrame *)
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
    void operator()(const SaCapRouterIPv4 *p)
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


// Get an entry for addr from map.
//
static inline IpAddrEntry &getIpAddrEntry(IpAddrMap &map, 
					  const IpAddress &addr)
{
  // only works for host entries
  
  IpAddrMap::iterator pEntry = map.find(addr);

  if (pEntry == map.end()) {  
    pEntry = map.find(IpAddress::AllZeros);
    assert (pEntry != map.end()); // assume a default entry always exists
  }

  return *pEntry;
}


// Search the IpAddrMap for a matching entry, if one is found
// save the cap, add the flow to the entry, and push a 
// deleter on the flow
//
// If no match is found in the IpAddrMap create a flow to the
// endstation and let it handle the flow
//
bool SaRouterStub::forward(SaFlow *fl, 
			   const SaCapRouterIPv4 *inCap,
			   const IpAddress *ipd,
			   int,
			   SaCapRouterIPv4 *&outCap,
			   IpAddress &ipnh)
{
  
  IpAddrEntry &ipEntry = getIpAddrEntry(*itsIpAddrMap, *ipd);
  assert (nextHopAddr(ipEntry));

  outCap = cap(ipEntry);
  ipnh = nextHopAddr(ipEntry);
  
  MapInsertResult<Flows> result = ipFlows(ipEntry).insert(fl);
  assert(result.ok());
    
  Deleter *pDeleter = new Deleter(*itsIpAddrMap, *ipd);
  assert(pDeleter);
  fl->push(pDeleter);

  cout << BEGINFORWARD SPACE BEGINSTRING << *ipd
       << ENDSTRING SPACE BEGINSTRING << ipnh 
       << ENDSTRING SPACE << *inCap 
       << SPACE << *outCap 
       << SPACE << fl << ENDFORWARD;
  
  return true;
}


// Show a IpAddrMap map entry as list who's car is the string
// representation of the destination IP address and who's
// cdr is the next hop IP adddess, destination cap, and a list 
// of flows to that destination.
//
class ShowIpAddrEntry
{
    ostream &stream;
    const char *sep;
public:
    void operator()(IpAddrEntry &n)
    {
        stream << sep << OPEN BEGINSTRING << keyAddr(n) << ENDSTRING SPACE
	       << BEGINSTRING << nextHopAddr(n) << ENDSTRING << SPACE;

	stream << *cap(n);

        stream << SPACE OPEN;
        forAll(ipFlows(n), ShowFlow(stream));
        stream << CLOSE CLOSE;
        sep = SPACE;
    }
    ShowIpAddrEntry(ostream &s): stream(s), sep("") {}
};


// Dump path maps.
//
SaRouterStub::~SaRouterStub()
{
    cout << endl << "Destructing router:" << endl;
    cout << BEGINROUTER SPACE OPEN;
    forAll(*itsCaps, ShowCap(cout));
    cout << CLOSE SPACE OPEN;
    forAll(*itsIpAddrMap, ShowIpAddrEntry(cout));
    cout << CLOSE << ENDROUTER << endl;
    delete itsIpAddrMap;
    delete itsCaps;
}

// Backdoor to add a route to the router's path map
//
bool SaRouterStub::addRoute( const IpAddress &nh, const Caps &c, 
			     const IpAddress &key )
{
  // don't allow duplicate entiries
  //
  IpAddrMap::iterator pEntry = itsIpAddrMap->find(key);
  assert(pEntry == itsIpAddrMap->end());

  // only allow unicast -- single cap -- entries for now
  //
  IpAddrEntry entry(key, AddrCapFlows(nh, CapFlows(c[0], Flows())));
  MapInsertResult<IpAddrMap> result = itsIpAddrMap->insert(entry);

  return result.ok();

}

// Add a new IP interface to the router with IfNumber ifNum.
// Use arp as the interface to resolve IpAddresses to MacAddresses
//
SaCapRouterIPv4 *SaRouterStub::addIf( int ifNum, SaArpEnet &arp )
{
  // figure out which SaNodeCsmaCd is this ifNum
  SaNodeCsmaCd *csmaCdNode = itsIfTranslator->translate(ifNum);
  assert(csmaCdNode);
  
  // build the node graph to support an IPv4 router interface
  SaCapRouterIPv4 *pCap = csmaCdNode->addIPv4(arp);
  assert(pCap);
  pCap->addUpper(*this);
  itsCaps->push_back(pCap);
  return pCap;
}


