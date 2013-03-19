#ifndef SA_SELECT_HH_INCLUDED
#define SA_SELECT_HH_INCLUDED



#include <assert.h>
#include <sys/types.h>
#include <sys/time.h>



// SunOS headers don't declare these!  We need bzero() for the damn
// FD_ macros defined for use with select().  Recognize that bzero()
// is a deprecated Berkeleyism.  memset(), or /dev/zero or something,
// is more portable.
//
extern "C" int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);
extern "C" void bzero(char *, int);


// An STL-style bidirectional iterator on a FileDescriptorSet.
//
class FileDescriptorSetIterator
{
    friend class FileDescriptorSet;

    typedef FileDescriptorSetIterator iterator;

    FileDescriptorSet *itsSet;
    int itsFd;

    FileDescriptorSet *set() { return itsSet; }
    int fd() const { return itsFd; }

    friend bool operator==(const iterator &lhs, const iterator &rhs)
    {
        return lhs.itsSet == rhs.itsSet && lhs.itsFd == rhs.itsFd;
    }

    FileDescriptorSetIterator(FileDescriptorSet *set, int fd):
        itsSet(set), itsFd(fd)
    {}

public:

    FileDescriptorSetIterator(): itsSet(0), itsFd(-1) {}

    // Return true if this is actually on a file descriptor, and can
    // be safely dereferenced.  Return false if this is explicitly not
    // on a file descriptor, and was not initialized on any
    // FileDescriptorSet.  In STL parlance, return false if this has a
    // "singular" value, and true if this is "non-singular".
    //
    operator bool() const
    {
        static const iterator nil;
        return (*this) != nil;
    }

    // Return the file descriptor that this iterator is on.
    //
    int operator*() const { assert(*this); return itsFd; }

    // Move this iterator forward on its set.
    //
    FileDescriptorSetIterator &operator++();
    FileDescriptorSetIterator operator++(int);

    // Move this iterator backward on its set.
    //
    FileDescriptorSetIterator &operator--();
    FileDescriptorSetIterator operator--(int);
};



// Iterate forward over file descriptors in a set from INIT to the end.
//
#define SA_FDS_FOR_REST(FD, INIT)\
    int FD; for (FD = (INIT); (FD) < FD_SETSIZE; ++(FD))

// Iterate forward over all file descriptors in a set.
//
#define SA_FDS_FOR_ALL(FD) SA_FDS_FOR_REST(FD, 0)



// A collection of file descriptors defined to support some useful STL
// algorithms on set class templates, along with some of the
// operations usually implemented with select() and its associated
// macro hackery.
//
// A FileDescriptorSet is different from the usual STL set in that all
// FileDescriptorSet objects are subsets of some finite set containing
// all file descriptors of integer value 0 through max_size().  In
// other words, this is just a wrapper around a bitfield, so we define
// some extra members like set(), clear(), operator~(), and bool for
// convenience.
//
class FileDescriptorSet
{
    fd_set itsFds;

    enum { badSelect = -1 };

public:

    typedef FileDescriptorSetIterator iterator;
    typedef iterator const_iterator;
    typedef int key_type;
    typedef int value_type;
    typedef int size_type;

    // Empty this set.  "zero()" is obviously the wrong name for this
    // member if we really want to take a set-theoretic approach, but
    // I can't think of a better one other than empty() which STL
    // usurps for a common predicate on collections.  [-Perhaps
    // makeEmpty() would be better, but it would probably just confuse
    // network hackers and make me appear even more fussy than I am!-]
    //
    void zero() { FD_ZERO(&itsFds); }

    // This should probably be called add() or something, but see above.
    // ["Add()" suggests arithmetic addition to C programmers.]
    //
    FileDescriptorSet &set(int fd) { FD_SET(fd, &itsFds); return *this; }

    // This should probably be called remove(), but see above, so we
    // name it for the traditional dual of set().
    //
    FileDescriptorSet &clear(int fd) { FD_CLR(fd, &itsFds); return *this; }

    // This basically tests whether fd is in this.
    //
    bool operator[](int fd) const { return FD_ISSET(fd, &itsFds); }

    // Return the least file descriptor in this whose integer value is
    // greater than fd.  Return end() if there is no file descriptor
    // in this greater than fd.
    //
    int next(int fd) const
    {
        int start = fd + 1;
        SA_FDS_FOR_REST(n, start) if ((*this)[n]) break;
        return n;
    }

    // Return the greatest file descriptor in this whose integer value
    // is less than fd.  Return end() if there is no file descriptor
    // in this whose integer value is less than fd.
    //
    int prev(int fd) const
    {
        int result = FD_SETSIZE;
        SA_FDS_FOR_ALL(n) {
            if (n == fd) {
                return result;
            } else {
                if ((*this)[n]) result = n;
            }
        }
        return FD_SETSIZE;
    }

    // Return true if this and that have the same members.  Return
    // false if they do not.
    //
    bool equals(const FileDescriptorSet &that) const
    {
        SA_FDS_FOR_ALL(n) if ((*this)[n] != that[n]) return false;
        return true;
    }

    // Return the least fd in this.  Return end() if there is no fd in
    // this.
    //
    iterator begin() {
        SA_FDS_FOR_ALL(fd) if ((*this)[fd]) break;
        return iterator(this, fd);
    }

    // Return the one-past-the-end iterator on this.  Note that the
    // iterator returned by end() is "singular".  Don't dereference it!
    //
    iterator end() { return iterator(this, FD_SETSIZE); }

    // Return true if this is empty.  Return false otherwise.
    //
    bool empty() const {
        static const FileDescriptorSet theEmptySet;
        return this->equals(theEmptySet);
    }

    // Return the number of file descriptors in this.
    //
    size_type size() const
    {
        size_type count = 0;
        SA_FDS_FOR_ALL(n) if ((*this)[n]) ++count;
        return count;
    }

    // Return the number of file descriptors this can hold.  (Not very
    // sety is it?-)
    //
    static size_type max_size() { return FD_SETSIZE; }

    // Put fd in this.  This is an idempotent operation.
    //
    FileDescriptorSet &insert(value_type fd)
    {
        assert(fd < max_size());
        set(fd);
        return *this;
    }

    // Remove fd from this.  This is an idempotent operation too.
    //
    FileDescriptorSet &erase(value_type fd)
    {
        assert(fd < max_size());
        clear(fd);
        return *this;
    }

    // This is handy sometimes, and is something network hackers like
    // to do.
    //
    operator bool() const { return !empty(); }

    // Make this have the same members as that.
    //
    FileDescriptorSet &operator=(const FileDescriptorSet &that)
    {
        if (this != &that) {
            zero();
            SA_FDS_FOR_ALL(n) if (that[n]) set(n);
        }
        return *this;
    }

    FileDescriptorSet(const FileDescriptorSet &that) { (*this) = that; }

    // This has the fdCount file descriptors in fds, and only those.
    //
    FileDescriptorSet(int fdCount, int fds[])
    {
        assert(fdCount < max_size());
        zero();
        for (int n = 0; n < fdCount; ++n) {
            assert(fds[n] < max_size());
            set(fds[n]);
        }
    }

    // Construct an empty file descriptor set.
    //
    FileDescriptorSet() { zero(); }

    // Return the complement of this.
    //
    FileDescriptorSet operator~() const
    {
        FileDescriptorSet result;
        SA_FDS_FOR_ALL(n) if (!(*this)[n]) result.set(n);
        return result;
    }

    // Add the elements of that to this.  In other words, make this be
    // the set union of this and that.
    //
    FileDescriptorSet &operator|=(const FileDescriptorSet &that)
    {
        if (this != &that) {
            SA_FDS_FOR_ALL(n) if (that[n]) set(n);
        }
        return *this;
    }

    // Remove the elements of this that aren't in that.  In other
    // words, make this be the set intersection of this and that.
    //
    FileDescriptorSet &operator&=(const FileDescriptorSet &that)
    {
        if (this != &that) {
            SA_FDS_FOR_ALL(n) if (that[n]) set(n); else clear(n);
        }
        return *this;
    }

    // Remove the elements of this that are in that.  In other words,
    // make this be the set difference of this and that.
    //
    FileDescriptorSet &operator^=(const FileDescriptorSet &that)
    {
        if (this == &that) {
            zero();
        } else {
            SA_FDS_FOR_ALL(n) {
                if ((*this)[n] == that[n]) clear(n); else set(n);
            }
        }
        return *this;
    }

    // Return when some file descriptor in this is ready for read().
    // Return the set of all file descriptors in this that will not
    // block in read().  This is essentially a blocking read select().
    //
    FileDescriptorSet selectRead() const
    {
        FileDescriptorSet result(*this);
        int status = ::select(max_size(), &result.itsFds, 0, 0, 0);
        assert(status != badSelect);
        return result;
    }

    // When you need selectWrite() and so on, implement them here.
};


#undef SA_FDS_FOR_ALL
#undef SA_FDS_FOR_REST


static inline bool operator==(
    const FileDescriptorSet &lhs, const FileDescriptorSet &rhs)
{
    return lhs.equals(rhs);
}

#define SA_FDS_BINARY_OP(OP)\
    static inline FileDescriptorSet operator OP (\
        const FileDescriptorSet &lhs, const FileDescriptorSet &rhs\
    )\
    {\
        FileDescriptorSet result(lhs);\
        return result OP ## = rhs;\
    }
SA_FDS_BINARY_OP(|);
SA_FDS_BINARY_OP(&);
SA_FDS_BINARY_OP(^);
#undef SA_FDS_BINARY_OP


inline FileDescriptorSet::iterator &FileDescriptorSet::iterator::operator++()
{
    assert(*this);
    itsFd = itsSet->next(itsFd);
    return *this;
}
inline FileDescriptorSet::iterator FileDescriptorSet::iterator::operator++(int)
{
    assert(*this);
    iterator result(*this);
    ++(*this);
    return result;
}
inline FileDescriptorSet::iterator &FileDescriptorSet::iterator::operator--()
{
    assert(*this);
    itsFd = itsSet->prev(itsFd);
    return *this;
}
inline FileDescriptorSet::iterator FileDescriptorSet::iterator::operator--(int)
{
    assert(*this);
    iterator result(*this);
    --(*this);
    return result;
}


#endif // ! SA_SELECT_HH_INCLUDED
