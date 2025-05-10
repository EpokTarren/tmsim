tmsim:
	ghc -outputdir build -o build/tmsim -O2 src/*.hs


ifeq ($(OS),Windows_NT)
clean:
	rmdir /S /Q build

run: tmsim
	build\tmsim.exe
else
clean:
	rm -r build

run: tmsim
	./build/tmsim
endif
