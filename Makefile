all: walkabout
walkabout: walkabout.hs Geography.hs
	ghc --make -O2 walkabout.hs -o walkabout
