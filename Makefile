

# This just redirects you to the ./src/ subdirectory.

default:
	(cd src/; $(MAKE) )

wsparse:
	(cd src/; $(MAKE) wsparse)

chez:
	(cd src/; $(MAKE) chez)

ik:
	(cd src/; $(MAKE) ik)

pltbc:
	(cd src/; $(MAKE) pltbc)


