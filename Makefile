tmsim:
	ghc -outputdir build -o tmsim -O2 tmsim.hs

run: tmsim
	tmsim

ifeq ($(OS),Windows_NT)
clean:
	if exist build     (rmdir /S /Q build)
	if exist tmsim     (del tmsim)
	if exist tmsim.exe (del tmsim.exe)
else
clean:
	[ -e build     ] && rm -r build  || 0
	[ -e tmsim     ] && rm tmsim     || 0
	[ -e tmsim.exe ] && rm tmsim.exe || 0
endif
