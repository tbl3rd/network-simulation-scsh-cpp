#ifndef ARP_ENET_STUB_HH_INCLUDED
#define ARP_ENET_STUB_HH_INCLUDED

#include <arp_enet.hh>
#include <ipaddress.hh>
#include <macaddress.hh>

#include <set>

class SaFlow;

template<class T> class less;
template<class Key, class Value, class Compare> class map;

// A stub implementation of an Ethernet ARP cache
//
class SaArpEnetStub:
    public SaArpEnet
{
  // a pseudo-ARP cache
  typedef set<SaFlow *, less<const SaFlow *> > Flows;
  typedef pair<MacAddress, Flows> MacFlows;
  typedef map<IpAddress, MacFlows, less<IpAddress> > ArpMap;

  ArpMap *itsArpMap;

public:

  ~SaArpEnetStub();

  SA_ARP_ENET_MEMBERS;

  // an ARP cache
  //
  SaArpEnetStub();

  // backdoors to the arp map for testing
  //
  bool addMap( const IpAddress &nh, const MacAddress &m );

  bool deleteMap( const IpAddress &key );

};

#endif // ! ARP_ENET_STUB_HH_INCLUDED
