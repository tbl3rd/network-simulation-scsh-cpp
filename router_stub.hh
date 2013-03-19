#ifndef ROUTER_STUB_HH_INCLUDED
#define ROUTER_STUB_HH_INCLUDED

#include <router_ipv4.hh>

#include <set>

class SaArpEnet;
class SaIfTranslator;

template<class T> class less;
template<class Key, class Value, class Compare> class map;


// A stub implementation of an IPv4 router w/static path tables
//
class SaRouterStub:
    public SaRouterIPv4
{
  // for an IPv4 router path table
  typedef set<SaFlow *, less<const SaFlow *> > Flows;
  typedef pair<SaCapRouterIPv4 *, Flows> CapFlows;
  typedef pair<IpAddress, CapFlows> AddrCapFlows;
  typedef map<IpAddress, AddrCapFlows, less<IpAddress> > IpAddrMap;

  Caps *itsCaps;
  IpAddrMap *itsIpAddrMap;
  const SaIfTranslator * const itsIfTranslator;


public:

  ~SaRouterStub();

  SA_ROUTER_IPV4_MEMBERS;
  
  // Create a router stub w/o caps.
  //
  SaRouterStub( const SaIfTranslator &t );

  // Backdoor to the path map for testing.
  //
  bool addRoute( const IpAddress &nh, 
		 const Caps &c,
		 const IpAddress &key = IpAddress::AllZeros);

  bool deleteRoute( const IpAddress &key = IpAddress::AllZeros );
  
  // Add a new IP interface to the router.
  //
  SaCapRouterIPv4 *addIf( int ifNum, SaArpEnet &arp );

  // Delete an IP interface.
  //
  void deleteIf( int ifNum );

};


#endif // ! ROUTER_STUB_HH_INCLUDED

