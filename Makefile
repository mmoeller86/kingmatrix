FORTRAN=gfortran
FFLAGS=

OBJS=gen.o \
eispack/cbabk2.o \
eispack/cbal.o \
eispack/cdiv.o \
eispack/cg.o \
eispack/comqr.o \
eispack/comqr2.o \
eispack/corth.o \
eispack/csroot.o \
eispack/pythag.o

all: gen

gen.o: gen.f95
	$(FORTRAN) -c $(FFLAGS) $? -o $@

.f.o:
	$(FORTRAN) -c $(FFLAGS) $? -o $@

gen: $(OBJS)
	$(FORTRAN) -o $@ $(OBJS)

clean:
	rm -f $(OBJS) gen.exe
