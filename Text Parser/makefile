all: remove
	make ParseFile
	make ParseFolder

ParseFile:
	ghc -O2 "ParseFile.hs" -i../

Profile:
	ghc -O2 "ParseFile.hs" -i../ -prof -fprof-auto -rtsopts

ParseFolder:
	ghc -O2 "ParseFolder.hs" -i../

clean:
	rm -f *.hi
	rm -f *.o

remove:
	rm -f ParseFile
	rm -f ParseFolder
