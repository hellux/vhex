.POSIX:
.SUFFIXES: .hs

CFLAGS = -Wall -fno-warn-unused-do-bind -fwarn-tabs -threaded -O2 \
		 -outputdir ${OBJDIR}

OBJDIR = build

vhex: VHex

.hs:
	ghc ${CFLAGS} -o $@ $<
