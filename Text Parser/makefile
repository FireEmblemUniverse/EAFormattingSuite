all: remove ParseFile ParseFileJIS ParseFolder

ParseFile: removeParser FEParser
	ghc -O2 "ParseFile.hs" -i../ -o "ParseFile"

ParseFileSJIS: removeParser FEParserSJIS
	ghc -O2 "ParseFile.hs" -iSJIS/:../ -o "ParseFileJIS"

removeParser: 
	rm -f "../FEParser.hi"

FEParserSJIS: GBAUtilities
	ghc -O2 -c "SJIS/FEParser.hs" -i../ -ohi "SJIS/FEParser.hi"

FEParser: GBAUtilities Gringe
	ghc -O2 -c "../FEParser.hs" -i../ -ohi "../FEParser.hi"

GBAUtilities:
	ghc -O2 -c "../GBAUtilities.hs" -i../

Gringe:
	ghc -O2 -c "../Gringe.hs"

Profile:
	ghc -O2 "ParseFile.hs" -i../ -prof -fprof-auto -rtsopts

ParseFolder:
	ghc -O2 "ParseFolder.hs" -i../

clean:
	rm -f *.hi
	rm -f *.o

remove:
	rm -f ParseFile
	rm -f ParseFileJIS
	rm -f ParseFolder
