#include <assert.h>

#include <if_translator.hh>
#include <node_csma_cd.hh>

#include <algorithm>
#include <map>

typedef map<int, SaNodeCsmaCd *, less<int> > NodeMap;
typedef NodeMap::value_type NodeMapEntry;


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
// elements of a NodeMap.
//
static inline int ifNum(NodeMapEntry &n) { return n.first; }
static inline SaNodeCsmaCd *node(NodeMapEntry &n) { return n.second; }


SaIfTranslator::~SaIfTranslator()
{
  delete itsMap;
}

SaIfTranslator::SaIfTranslator(): itsMap(0) 
{
  itsMap = new NodeMap;
}

// Translate IfNumber to SaNodeCsmaCd.
//
SaNodeCsmaCd *SaIfTranslator::translate (int IfNum) const
{
  return node(*itsMap->find(IfNum));
}

// Add a translation.
// 
void SaIfTranslator::record (int ifNum, SaNodeCsmaCd *node)
{
  NodeMapEntry entry(ifNum, node);
  MapInsertResult<NodeMap> result = itsMap->insert(entry);
  assert(result.ok());
}

// Remove a translation.
//
void SaIfTranslator::erase (int ifNum)
{
  itsMap->erase(ifNum);
}
