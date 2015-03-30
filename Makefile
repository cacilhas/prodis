# TODO: make it multiplataform
PROLOG= /Applications/SWI-Prolog.app/Contents/MacOS/swipl --quiet
PROLOGC= $(PROLOG) -O --toplevel=halt.
RM= rm -f


#-------------------------------------------------------------------------------
all: prodis


prodis: prodis.pl
	$(PROLOGC) --goal=start_prodis. -o $@ -c $<


debug: prodis.pl
	$(PROLOG) -f $< -g gxref.


.PRONY: clean
clean:
	$(RM) prodis
	$(RM) *~
