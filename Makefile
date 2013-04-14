all: interpreter C++Front hsFront

interpreter: interpreter.hs Parser.hs
	ghc -Wall -O2 --make -o $@ $^

C++Front: C++Front.hs StringQQ.hs Parser.hs
	ghc -Wall -O2 --make -o $@ $^

hsFront: hsFront.hs StringQQ.hs Parser.hs
	ghc -Wall -O2 --make -o $@ $^

clean :
	-rm -f interpreter C++Front hsFront *.hi *.o
