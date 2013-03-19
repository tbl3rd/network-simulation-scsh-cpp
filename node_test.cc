#include <strstream.h>

#include <algorithm>
#include <vector>

#include <test_util.hh>
#include <generic.hh>
#include <node.hh>
#include <type.hh>


// Create a simple tree.  Return a string representation of the tree.
// Destroy the tree.
//
static char *dump()
{
    const int capCount = 23;
    SaGeneric anchor;
    for (int n = 0; n < capCount; ++n) {
        SaGeneric *pG = new SaGeneric(anchor);
        assert(pG);
    }

    cout << endl << '(' << endl;
    DumpId dumpIt(cout);
    SaNode::find(dumpIt);
    cout << ')' << endl;

    ostrstream s;
    anchor.dump(s);
    s << ends;
    cout << s.str() << endl;

    return s.str();
}

static void restore(char *p)
{
    istrstream s(p);
    SaNode *pNode = SaNode::restore(s);
    SaGeneric *pGeneric = SA_DYNAMIC_CAST(SaNode, SaGeneric, pNode);
    assert(pGeneric);

    cout << endl << endl;
    pGeneric->dump(cout);
    cout << endl << endl;

    cout << SHOW( pGeneric) << endl;
    cout << SHOW(*pGeneric) << endl;
    cout << SHOW( pGeneric->level()) << endl;
    cout << SHOW( pGeneric->degree()) << endl;
    cout << SHOW( pGeneric->root()) << endl;
    cout << SHOW(*pGeneric->root()) << endl;
    cout << SHOW( pGeneric->type()) << endl;

    assert(pGeneric == pGeneric->root());

    delete[] p;
    delete pNode;
}

int main(int, char *[])
{
    restore(dump());
    streamToGraph(cin);
    graphToStream(cout);
    testPath();
    return 0;
}
