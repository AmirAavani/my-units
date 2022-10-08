clean:
	rm -f General-Purpose-Units/*.ppu General-Purpose-Units/*.o 
	rm */*.ppu */*.o 
	cd SAT-Solver-Units/  && make clean
	cd PBSolverEngine && make clean
	cd Factorization && make clean
