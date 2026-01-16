FC=f77 -g
FD=f77
F95=gfortran
OEXT=.o
EEXT=
RM=rm -rf
ROOT=
FFLAGS=
ROOT=
FDFLAGS=-L$(ROOT)/usr/lib -L$(PGPLOT_DIR) -Wl,-rpath=$(PGPLOT_DIR)
LIBS=-lpgplot -lpng -lz -lX11
SRCS=
OBJS = $(SRCS:.f=$(OEXT))
TARGETS = $(OBJS:$(OEXT)=$(EEXT)) circle001$(EEXT) atoms$(EEXT)
.PHONY: all clean

all: $(OBJS) $(TARGETS)

.f.o:
	$(FC) -c $(FFLAGS) -o $@ $<

.o:
	$(FD) -o $@ $^ $(LIBS) $(FDFLAGS)

circle001$(EEXT): circle001.f95
	$(F95) $< -o $@

atoms.o: atoms.f95
	$(F95) -c $<
atoms$(EEXE): atoms.o random.o system.o
	$(F95) $^ -o $@

clean:
	$(RM) *.o $(OBJS) $(TARGETS) circle001$(EEXT) fort.*

