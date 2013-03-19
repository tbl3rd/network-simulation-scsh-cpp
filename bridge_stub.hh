#ifndef BRIDGE_STUB_HH_INCLUDED
#define BRIDGE_STUB_HH_INCLUDED

#include <bridge.hh>
#include <macaddress.hh>

#include <set>

class SaFlow;
class SaCapBridge;

template<class T> class less;
template<class Key, class Value, class Compare> class map;


// A stub implementation of an unmanaged learning bridge.
//
class SaBridgeStub:
    public SaBridge
{
    typedef set<SaFlow *, less<const SaFlow *> > Flows;
    typedef pair<SaCapBridge *, Flows> CapFlows;
    typedef map<MacAddress, CapFlows, less<MacAddress> > MacMap;

    Caps *itsCaps;
    MacMap *itsMacMap;

public:

    ~SaBridgeStub();

    SA_BRIDGE_MEMBERS;

    // Bridge over the caps in domain.
    //
    SaBridgeStub(const Caps &domain);
};


#endif // ! BRIDGE_STUB_HH_INCLUDED
