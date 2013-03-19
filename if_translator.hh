#ifndef IF_TRANSLATOR_HH_INCLUDED
#define IF_TRANSLATOR_HH_INCLUDED

class SaNodeCsmaCd;

template<class T> class less;
template<class Key, class Value, class Compare> class map;

// A mapping of IfNumbers (ints) to SaNodeCsmaCds
//
class SaIfTranslator
{
  typedef map<int, SaNodeCsmaCd *, less<int> > NodeMap;

  NodeMap *itsMap;

public:

  ~SaIfTranslator();

  SaIfTranslator();

  // Translate IfNumber to SaNodeCsmaCd.
  //
  SaNodeCsmaCd *translate (int IfNum) const;

  // Add a translation.
  // 
  void record (int IfNum, SaNodeCsmaCd *node);

  // Remove a translation.
  //
  void erase (int IfNum);

};


#endif // ! IF_TRANSLATOR_HH_INCLUDED

