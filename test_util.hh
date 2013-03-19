#include <assert.h>
#include <iostream.h>
#include <node.hh>


#define SHOW(X) #X " == " << X


class DumpId:
    public SaNode::Predicate
{
    ostream &out;
public:
    bool operator()(SaNode *pN)
    {
        out << '(' << pN << ' ' << pN->id() << ')';
        return false;
    }
    DumpId(ostream &o): out(o) {}
};


static void showNode(SaNode *pN)
{
    assert(pN);
    cout << endl << SHOW(*pN) << endl
         << SHOW(pN->level()) << endl
         << SHOW(pN->degree()) << endl;
    cout << "Dumping node: " << pN->id() << endl;
    pN->dump(cout);
    cout << endl;
}


// If we had a proper Graph class, this function would be one of its
// ctors.  But we don't (yet) so we use this kluge instead.  Sorry.
//
static inline void streamToGraph(istream &s)
{
    SaNode::Vector anchors = SaNode::restoreGraph(s);
    cout << endl << SHOW(anchors.size()) << endl;
    for_each(anchors.begin(), anchors.end(), showNode);
}


// If we had a proper Graph class, this would be a member.
// Sorry again.
//
static inline void graphToStream(ostream &s)
{
    s << endl << "Dumping graph:" << endl;
    SaNode::dumpGraph(s);
    s << endl;
}


struct LeafFinder: SaNode::Predicate
{
    bool operator()(SaNode *pNode) { return 0 == pNode->degree(); }
};

static SaNode *findLeaf()
{
    cout << endl << "Finding a leaf node ...";
    LeafFinder lf;
    SaNode *pLeaf = SaNode::find(lf);
    cout << " done." << endl;
    cout << SHOW(pLeaf) << endl;
    if (pLeaf) {
        cout << SHOW(*pLeaf) << endl
             << SHOW(pLeaf->degree()) << endl
             << SHOW(pLeaf->level()) << endl;
    }
    return pLeaf;
}

static inline void testPath()
{
    SaNode *pLeaf = findLeaf();
    if (pLeaf) {
        cout << "Found a leaf at: " << pLeaf << endl;
        SaNode::Vector path;
        pLeaf->path(path);
        cout << "Dumping a path:" << endl;
        cout << SHOW(path.size()) << endl;
        for_each(path.begin(), path.end(), showNode);
        cout << endl;
        cout << SHOW(pLeaf->root()) << endl;
        cout << SHOW(*pLeaf->root()) << endl;
        cout << endl;
    }
}
