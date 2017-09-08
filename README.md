# MAN-AHL
Commands

To build and profile :
cd src/
ghc -prof -fprof-auto -rtsopts -main-is Proba.Run.main Proba/Run.hs -O2
./Proba/Run +RTS -p -s
cat Run.prof 


cabal build
time ./dist/build/Proba/Proba -run=Weighted -nSims=1000000 +RTS -s
time cabal test --show-details=streaming

