all: interpreter C++Front C++Front2 hsFront

interpreter: interpreter.hs Transformer.hs Parser.hs
	ghc -Wall -O2 --make -o $@ $^

C++Front: C++Front.hs StringQQ.hs Parser.hs
	ghc -Wall -O2 --make -o $@ $^

C++Front2: C++Front2.hs Transformer.hs StringQQ.hs Parser.hs
	ghc -Wall -O2 --make -o $@ $^

hsFront: hsFront.hs StringQQ.hs Parser.hs
	ghc -Wall -O2 --make -o $@ $^

clean :
	-rm -f interpreter C++Front C++Front2 hsFront *.hi *.o
