trapojack: filestore-dirs executive
	ghc -o trapojack Main.hs

executive: executive.cpp

filestore-dirs:
	mkdir -p filestore/inputs filestore/solutions

.FORCE: filestore-dirs
.PHONY: filestore-dirs
