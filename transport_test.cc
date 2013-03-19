#include <iostream.h>
#include <strstream.h>

#include <address_ip_socket.hh>
#include <sa_message_transport.hh>
#include <socket.hh>


#define SPACE " "
#define TAB "\t"


// A command line for this program.
//
class CommandLine
{
    int itsCount;
    int itsAc;
    char **itsAv;
    SaAddressIpSocket itsAddress;
    bool itsOk;

public:

    friend ostream &operator<<(ostream &s, const CommandLine &cl)
    {
        const char *space = "";
        for (int n = 0; n < cl.itsAc; ++n) {
            s << space << cl.itsAv[n];
            space = " ";
        }
        return s;
    }

    bool ok() const { return itsOk; }

    int count() const { return itsCount; }

    bool boot() const { return itsAc > 3; }

    const SaAddressIpSocket &address() const { return itsAddress; }

    void usage(ostream &s)
    {
        SaSocketIpTcp socket;
        static const SaAddressIpSocket address(socket.receiveAddress());

        s << "Usage:" TAB << itsAv[0] << " <WKA> <N> <bootFlag>"
          << endl << endl;

        s << "Where:" TAB "<WKA> is an IP socket address" << endl
          << TAB "in dotted decimal notation followed by a colon" << endl
          << TAB "and a port number." << endl << endl
          << TAB "<N> is a count of the number of processes" << endl
          << TAB "participating in the transport." << endl << endl
          << TAB "<bootFlag> is an argument that, if present," << endl
          << TAB "designates this process to boot the transport." << endl
          << endl
          << TAB "Exactly one process must get the boot flag." << endl
          << TAB "Exactly <N> - 1 processes must be invoked" << endl
          << TAB "without the boot flag.  All processes participating" << endl
          << TAB "in the transport must get the same <WKA> and <N>." << endl
          << endl;

        s << "Example 0: " << itsAv[0] << " \"" << address << "\" 2 boot"
          << endl
          << "Example 0: " << itsAv[0] << " \"" << address << "\" 2" << endl
          << endl;

        s << "Example 1: " << itsAv[0] << " \"" << address << "\" 3 boot"
          << endl
          << "Example 1: " << itsAv[0] << " \"" << address << "\" 3" << endl
          << "Example 1: " << itsAv[0] << " \"" << address << "\" 3" << endl;
    }

    CommandLine(int ac, char *av[]):
        itsCount(-1), itsAc(ac), itsAv(av), itsOk(false)
    {
        static const int addressIndex = 1;
        static const int countIndex = 2;
        itsOk = itsAc > countIndex;
        if (itsOk) {
            istrstream countStream(itsAv[countIndex]);
            itsOk = countStream >> itsCount;
            itsOk = itsOk && itsCount > 1;
            if (itsOk) {
                istrstream addressStream(itsAv[addressIndex]);
                itsOk = addressStream >> itsAddress;
            }
        }
    }
};


// Run some test on t.  Return true if it checks out.  Return false if
// the test fails.
//
bool validateTransport(SaMessageTransport &t)
{
    bool ok = true;
    const int channelCount = t.NumOfChannels();
    for (int channel = 0; ok && channel < channelCount; ++channel) {
        ok = t.ChannelIsValid(channel);
    }
    return ok;
}


int main(int ac, char *av[])
{
    CommandLine cl(ac, av);
    bool ok = cl.ok();

    if (ok) {

        SaAddressIpSocket a(cl.address());
        SaMessageTransport t(a, cl.count(), cl.boot());
        ok = t.NumOfChannels() + 1 == cl.count();
        ok = ok && validateTransport(t);
        cout << "Waiting for input: " << flush;
        cin.get();

    } else {

        cl.usage(cerr);
    }

    return ok == false;
}


// Stub out the Object Services runtime for the linker.
//
#include <message_port_id.hh>
size_t MessagePortId::_GlobalMpid = 0;
