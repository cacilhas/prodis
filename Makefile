UNAME= $(shell uname)

ifeq ($(UNAME), Darwin)
	# Mac OS X installation does not put swipl on PATH envvar
	PROLOG= /Applications/SWI-Prolog.app/Contents/MacOS/swipl --quiet
else
	PROLOG= swipl --quiet
endif

PROLOGC= $(PROLOG) -O --toplevel=halt.
TEST= $(PROLOG) -t run_tests.
RM= rm -f


#-------------------------------------------------------------------------------
all: prodis


prodis: prodis.pl
	$(PROLOGC) --goal=start_prodis. -o $@ -c $<


debug: prodis.pl
	$(PROLOG) -f $< -g gxref.


test:
	$(TEST) tests/*.pl


clean:
	$(RM) prodis
	find . -name "*~" -delete


.PHONY: clean test
