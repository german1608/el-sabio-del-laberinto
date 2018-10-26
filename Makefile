build:
	ghc *hs -o Sabio
run: build
	./Sabio
clean:
	find . -name "*.hi" -name ".o" -exec rm -rf {} +
