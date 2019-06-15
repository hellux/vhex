.POSIX:
.SUFFIXES: .hs

OBJDIR = build

vhex: VHex

.hs:
	ghc -threaded -O -outputdir ${OBJDIR} -o $@ $<
