program = test_inbox
test    = inbox-test.f08
mof     = inbox.f08
F90     = gfortran -std=f2023 -O3 -pedantic -Wall -Wextra
MF      = inbox_funcs.mod


$(program): $(mof) $(test)
	@echo "Compiling test:"
	$(F90) $(mof) $(test) -o $(program)
	@echo "Executing test:"
	./$(program)
	@echo "Cleaning files:"
	rm -f $(MF) $(program)
