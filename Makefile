# Nana Asiedu-Ampem (na1814)
# distributed algorithms, n.dulay, 4 jan 17
# simple build and run makefile, v1

.SUFFIXES: .erl .beam

MODULES  = acceptor client database leader server system

# BUILD =======================================================

ERLC	= erlc -o ebin

ebin/%.beam: %.erl
	$(ERLC) $<

all:	ebin ${MODULES:%=ebin/%.beam}

ebin:
	mkdir ebin

debug:
	erl -s crashdump_viewer start

.PHONY: clean
clean:
	rm -f ebin/* erl_crash.dump

# Run1 ===================================================

#L_HOST   = localhost.localdomain
L_ERL     = erl -noshell -pa ebin -setcookie pass

run:   all
	$(L_ERL) -s system start
