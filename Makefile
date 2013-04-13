all: lambda_cal translator

lambda_cal: lambda_cal.hs
	ghc -Wall -O2 --make -o lambda_cal lambda_cal.hs

translator: translator.hs StringQQ.hs
	ghc -Wall -O2 --make -o translator translator.hs StringQQ.hs

clean :
	-rm -f lambda_cal translator *.hi *.o
