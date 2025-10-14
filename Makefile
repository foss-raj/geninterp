program = InboxTest
MODS    = inbox.o
OBJ     = inbox-test.o
F90     = gfortran -std=f2023 -O3 -pedantic -Wall -Wextra


$(program): $(MODS) $(OBJ)
	$(F90) -o $@ $(MODS) $(OBJ)

%.o: %.f08
	$(F90) $< -o $@

MF = $(wildcard *.mod)
OF = $(wildcard *.o)

clean:
	rm -f $(MF) $(OF) $(program)
