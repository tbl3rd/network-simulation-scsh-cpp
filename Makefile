include ../include.mk

MAKEFILES = Makefile

HH_ELEMENTS =\
    arp_enet_stub.hh\
    bridge_stub.hh\
    if_translator.hh\
    process.hh\
    router_stub.hh\
    select.hh\
    test_util.hh\
    #

SCHEMESCRIPTS =\
    graph.scm\
    miscellaneous.scm\
    modules.scm\
    network.scm\
    octets.scm\
    run-test.scm\
    test-network.scm\
    #

TEST_ELEMENTS =\
    bridge_test.cc\
    node_test.cc\
    router_test.cc\
    socket_test.cc\
    #

LIBRARY_ELEMENTS =\
    arp_enet_stub.cc\
    bridge_stub.cc\
    if_translator.cc\
    router_stub.cc\
    #

CC_ELEMENTS =\
    $(TEST_ELEMENTS)\
    $(LIBRARY_ELEMENTS)\
    #

ELEMENTS =\
    $(MAKEFILES)\
    $(HH_ELEMENTS)\
    $(CC_ELEMENTS)\
    $(SCHEMESCRIPTS)\
    #

JUNK = $(ELEMENTS:%=%~)


PREFIX = SA_
include $(PROJECTSOURCE)/cc.mk
SA_OBJECTS = $(SA_CC:%.cc=%.o)

ADDRESSTYPESRC = $(ADDRESSTYPEROOT)/src
PREFIX = ADDRESSTYPE_
include $(ADDRESSTYPESRC)/cc.mk
ADDRESSTYPE_OBJECTS = $(ADDRESSTYPE_CC:%.cc=%.o)

ADDRESSTYPE_INCLUDES =\
    $(ADDRESSTYPESRC)\
    $(ADDRESSTYPEINCLUDE)\
    #

SA_INCLUDES :=\
    $(PROJECTSOURCE)\
    $(INCLUDES)\
    $(ADDRESSTYPEINCLUDE)\
    #

INCLUDES :=\
    .\
    $(SA_INCLUDES)\
    $(OBJECTSERVICESINCLUDE)\
    #


CXXFLAGS += -frepo

TEST_OBJECTS = $(TEST_ELEMENTS:%.cc=%.o)
LIBRARY_OBJECTS = $(LIBRARY_ELEMENTS:%.cc=%.o)
OBJECTS = $(CC_ELEMENTS:%.cc=%.o)

EXECUTABLES = $(TEST_ELEMENTS:%.cc=%)

TARGETS =\
    $(EXECUTABLES)\
    release\
    tests\
    #


default: tests
	$(MAKEBOGUS)

release: $(EXECUTABLES) $(SCHEMESCRIPTS) $(MAKEFILES)
	$(MAKEBOGUS)

tests: $(EXECUTABLES)
	$(MAKEBOGUS)

clean:
	$(RM) $(JUNK)
	$(RM) -r $(EXECUTABLES:%=%.d)

clobber: clean
	$(RM) $(TARGETS)


BRIDGE_TEST_OBJECTS :=\
    address_802_3.o\
    anchor.o\
    anchor_lan.o\
    bridge_stub.o\
    bridge_test.o\
    cap_bridge.o\
    cap_router_ipv4.o\
    decision.o\
    decision_bridge.o\
    decision_dot3_snap.o\
    decision_enet.o\
    decision_ipv4.o\
    decision_router_ipv4.o\
    demux_pid.o\
    flow.o\
    forwarder.o\
    id.o\
    node.o\
    node_802_3.o\
    node_csma_cd.o\
    node_ipv4.o\
    overlay_dot3_snap.o\
    overlay_enet.o\
    overlay_ipv4.o\
    type.o\
    $(ADDRESSTYPE_OBJECTS)\
#


    #
BRIDGE_TEST_OBJECTS :=\
    $(BRIDGE_TEST_OBJECTS:%=bridge_test.d/%)\
    #

bridge_test.d:
	$(RM) -r $@
	$(MKDIR) $@

bridge_test: %: %.d $(BRIDGE_TEST_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $(BRIDGE_TEST_OBJECTS)

$(OBJECTS:%=bridge_test.d/%): bridge_test.d/%.o: %.cc
	$(CXX) $(CXXFLAGS) $(INCLUDES:%=-I%) -c -o $@ $^

$(SA_OBJECTS:%=bridge_test.d/%): bridge_test.d/%.o: $(PROJECTSOURCE)/%.cc
	$(CXX) $(CXXFLAGS) $(SA_INCLUDES:%=-I%) -c -o $@ $^

$(ADDRESSTYPE_OBJECTS:%=bridge_test.d/%): bridge_test.d/%.o: $(ADDRESSTYPESRC)/%.cc
	$(CXX) $(CXXFLAGS) $(ADDRESSTYPE_INCLUDES:%=-I%) -c -o $@ $^

COLLECTOR=\
    /opt/cygnus/progressive-96q3/lib/gcc-lib/sparc-sun-sunos4.1.4/2.7-96q3/ld

bridge_test.pure: %.pure: % %.d $(BRIDGE_TEST_OBJECTS)
	/opt/purify/purify -collector=$(COLLECTOR)\
	$(LD) $(LDFLAGS) -o $@ $(BRIDGE_TEST_OBJECTS)


ROUTER_TEST_OBJECTS :=\
    anchor.o\
    anchor_lan.o\
    arp_enet_stub.o\
    cap_bridge.o\
    cap_router_ipv4.o\
    decision.o\
    decision_bridge.o\
    decision_dot3_snap.o\
    decision_enet.o\
    decision_ipv4.o\
    decision_router_ipv4.o\
    demux_pid.o\
    flow.o\
    forwarder.o\
    id.o\
    if_translator.o\
    node.o\
    node_csma_cd.o\
    node_ipv4.o\
    overlay_dot3_snap.o\
    overlay_enet.o\
    overlay_ipv4.o\
    router_stub.o\
    router_test.o\
    socket.o\
    type.o\
    $(ADDRESSTYPE_OBJECTS)\
    #

ROUTER_TEST_OBJECTS :=\
    $(ROUTER_TEST_OBJECTS:%=router_test.d/%)\
    #

router_test.d:
	$(RM) -r $@
	$(MKDIR) $@

router_test: %: %.d $(ROUTER_TEST_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $(ROUTER_TEST_OBJECTS)

$(OBJECTS:%=router_test.d/%): router_test.d/%.o: %.cc
	$(CXX) $(CXXFLAGS) $(INCLUDES:%=-I%) -c -o $@ $^

$(SA_OBJECTS:%=router_test.d/%): router_test.d/%.o: $(PROJECTSOURCE)/%.cc
	$(CXX) $(CXXFLAGS) $(SA_INCLUDES:%=-I%) -c -o $@ $^

$(ADDRESSTYPE_OBJECTS:%=router_test.d/%): router_test.d/%.o: $(ADDRESSTYPESRC)/%.cc
	$(CXX) $(CXXFLAGS) $(ADDRESSTYPE_INCLUDES:%=-I%) -c -o $@ $^

COLLECTOR=\
    /opt/cygnus/progressive-96q3/lib/gcc-lib/sparc-sun-sunos4.1.4/2.7-96q3/ld

router_test.pure: %.pure: % %.d $(ROUTER_TEST_OBJECTS)
	/opt/purify/purify -collector=$(COLLECTOR)\
	$(LD) $(LDFLAGS) -o $@ $(ROUTER_TEST_OBJECTS)



NODE_TEST_OBJECTS :=\
    decision.o\
    forwarder.o\
    generic.o\
    id.o\
    node.o\
    node_test.o\
    type.o\
    $(ADDRESSTYPE_OBJECTS)\
    #
NODE_TEST_OBJECTS :=\
    $(NODE_TEST_OBJECTS:%=node_test.d/%)\
    #

node_test.d:
	$(RM) -r $@
	$(MKDIR) $@

node_test: %: %.d $(NODE_TEST_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $(NODE_TEST_OBJECTS)

$(OBJECTS:%=node_test.d/%): node_test.d/%.o: %.cc
	$(CXX) $(CXXFLAGS) $(INCLUDES:%=-I%) -c -o $@ $^

$(SA_OBJECTS:%=node_test.d/%): node_test.d/%.o: $(PROJECTSOURCE)/%.cc
	$(CXX) $(CXXFLAGS) $(SA_INCLUDES:%=-I%) -c -o $@ $^

$(ADDRESSTYPE_OBJECTS:%=node_test.d/%): node_test.d/%.o: $(ADDRESSTYPESRC)/%.cc
	$(CXX) $(CXXFLAGS) $(ADDRESSTYPE_INCLUDES:%=-I%) -c -o $@ $^



SOCKET_TEST_OBJECTS :=\
    socket_test.o\
    socket.o\
    address_ip_socket.o\
    address_ip.o\
    #
SOCKET_TEST_OBJECTS :=\
    $(SOCKET_TEST_OBJECTS:%=socket_test.d/%)\
    #

socket_test.d:
	$(RM) -r $@
	$(MKDIR) $@

socket_test: %: %.d $(SOCKET_TEST_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $(SOCKET_TEST_OBJECTS)

$(OBJECTS:%=socket_test.d/%): socket_test.d/%.o: %.cc
	$(CXX) $(CXXFLAGS) $(INCLUDES:%=-I%) -c -o $@ $^

$(SA_OBJECTS:%=socket_test.d/%): socket_test.d/%.o: $(PROJECTSOURCE)/%.cc
	$(CXX) $(CXXFLAGS) $(SA_INCLUDES:%=-I%) -c -o $@ $^



$(MAKEFILES): ../include.mk
