build:
	ghc *hs -o Sabio
run: build
	./Sabio
clean:
	- rm *.o 2> /dev/null
	- rm *.hi 2> /dev/null
	- rm Sabio 2> /dev/null
