all: lambda_cal

lambda_cal: lambda_cal.hs
	ghc -Wall -O2 --make -o lambda_cal lambda_cal.hs

clean :
	-rm -f lambda_cal lambda_cal.hi lambda_cal.o
