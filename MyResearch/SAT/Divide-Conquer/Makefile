n ?= 1023
mode ?= BinSearch
output ?= bin

Run:
	./SQRT --n $(n) --Mode $(mode) --OutputFileName $(output).cnf --Verbosity 2 --EnableUP False > $(output).out 

Solve:
	./minisat_static $(output).cnf $(output).ans
