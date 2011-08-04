# Unix-Makefile for `eweb.lit'
#
# Note that `eweb.beam' must already exist for the `eweb'
# shell script to be able to execute EWEB in Erlang
# (assuming we run under BEAM).
#
# Be sure to have a backup of the latest stable `eweb.erl'
# (or `eweb.beam') file before running `make'!
# If the backup of `eweb.erl' is named `eweb.erl.stable',
# then just do `make restore' to get a working `eweb.beam'.
#
# The version of LaTeX used for typesetting `eweb.tex'
# should preferably be 2.09, rather than LaTeX 2e; the
# old version is generally named `latex209' if it has
# been kept on your local system.

#LATEX=latex209
LATEX=latex

STABLE=eweb.erl.stable
MACHINE=beam


all:	eweb.$(MACHINE) doc

doc:	eweb.dvi

ps:	eweb.ps

restore:	clean
	cp -pf $(STABLE) eweb.erl
	erl -compile eweb
	touch -r $(STABLE) eweb.$(MACHINE)

eweb.$(MACHINE):	eweb.erl
	erl -compile eweb

eweb.erl eweb.tex:	eweb.lit
	sh ./eweb

eweb.dvi:	eweb.tex
	$(LATEX) eweb.tex
	$(LATEX) eweb.tex

eweb.ps:	eweb.dvi
	dvips eweb.dvi -o

clean:
	-rm eweb.tex eweb.log eweb.aux eweb.erl

realclean:	clean
	-rm eweb.dvi
