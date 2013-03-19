#include <assert.h>

#include <arp_enet_stub.hh>
#include <cap_router_ipv4.hh>
#include <flow.hh>
#include <io.hh>
#include <ipaddress.hh>
#include <macaddress.hh>

#include <algorithm>
#include <map>
#include <set>

#define BEGINARPENET OPEN "arp cache"
#define ENDARPENET CLOSE
#define BEGINARP OPEN "arp"
#define ENDARP CLOSE

// for a map with entries which look like:
//
// IpAddress(key) -> MacAddress, Flows
//
typedef set<SaFlow *, less<const SaFlow *> > Flows;
typedef pair<MacAddress, Flows> MacFlows;
typedef map<IpAddress, MacFlows, less<IpAddress> > ArpMap;
typedef ArpMap::value_type ArpEntry;


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
// elements of an ArpMap.
//
static inline const IpAddress &ipAddr(ArpEntry &n) { return n.first; }
static inline const MacAddress &macAddr(ArpEntry &n) { return n.second.first; }
static inline Flows &flows(ArpEntry &n) { return n.second.second; }


// Cleaning up the arp cache state consists of finding the ArpEntry in the
// ArpMap corresponding to the saved address and, if it still exists,
// removing the flow from the flow set.  
//
class Deleter:
  public SaFlow::Deleter
{
  ArpMap &itsArpMap;
  IpAddress addr;
  
public:
  virtual void operator()(const SaFlow *pFlow)
  {
    ArpMap::iterator pEntry = itsArpMap.find(addr);
    
    if (pEntry != itsArpMap.end()) {
      SaFlow *const &rpFlow = pFlow; // help GCC sheesh!
      flows(*pEntry).erase(rpFlow);
    }
  }

  Deleter(ArpMap &arpMap, const IpAddress &key): 
    itsArpMap(arpMap), addr(key) {}
};


// create the arp cache
//
SaArpEnetStub::SaArpEnetStub(): itsArpMap(0)
{
    itsArpMap = new ArpMap;
}


// get an entry for addr from map
//
static inline ArpEntry &getArpEntry(ArpMap &map, const IpAddress &addr)
{
  ArpMap::iterator pEntry = map.find(addr);
  // hack until we can really Arp
  assert (pEntry != map.end()); 
  return *pEntry;
}

// resolve the IpAddress i to a MacAddress m
//
bool SaArpEnetStub::resolve(const IpAddress &i, const SaCapRouterIPv4 *, 
			    MacAddress &m, SaFlow *fl) 
{
  ArpEntry &a = getArpEntry(*itsArpMap, i);

  m = macAddr(a); 

  MapInsertResult<Flows> result = flows(a).insert(fl);
  assert(result.ok());

  Deleter *pDeleter = new Deleter(*itsArpMap, i);
  assert(pDeleter);
  fl->push(pDeleter);

  cout << BEGINARP SPACE BEGINSTRING << i
       << ENDSTRING SPACE BEGINSTRING << m
       << ENDSTRING SPACE << fl 
       << ENDARP;
    
  return true;
}


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


// Show a ArpMap map entry as list who's car is the string
// representation of the IP address and who's cdr is the 
// associated MAC adddess and a list of flows using the ARP
// entry.
//
class ShowArpEntry
{
  ostream &stream;
  const char *sep;
public:
  void operator()(ArpEntry &n)
  {
    stream << sep << OPEN BEGINSTRING << ipAddr(n) << ENDSTRING SPACE
	   << BEGINSTRING << macAddr(n) << ENDSTRING << SPACE;
    stream << OPEN;
    forAll(flows(n), ShowFlow(stream));
    stream << CLOSE CLOSE;
    sep = SPACE;
  }
  ShowArpEntry(ostream &s): stream(s), sep("") {}
};


// dump arp map
//
SaArpEnetStub::~SaArpEnetStub()
{
    cout << endl << "Destructing ARP cache:" << endl;
    cout << BEGINARPENET SPACE OPEN;
    forAll(*itsArpMap, ShowArpEntry(cout));
    cout << CLOSE << ENDARPENET << endl;
    delete itsArpMap;
}

// backdoor to add a mapping to the arp map
//
bool SaArpEnetStub::addMap(const IpAddress &nh, const MacAddress &m)
{
  // don't allow duplicate entiries
  //
  ArpMap::iterator pEntry = itsArpMap->find(nh);
  assert(pEntry == itsArpMap->end());

  ArpEntry entry(nh, MacFlows(m, Flows()));
  MapInsertResult<ArpMap> result = itsArpMap->insert(entry);
  
  return result.ok();
}

// void SaArpEnetStub::receive(SaFrame *) { assert(0); }

// void SaArpEnetStub::send(SaFrame *) { assert(0); }

