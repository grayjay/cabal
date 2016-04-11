. ./common.sh

cd p
cabal sandbox init > /dev/null

cabal sandbox add-source ../q > /dev/null

cabal install --only-dependencies > /dev/null
cabal configure > /dev/null
cabal build > /dev/null
./dist/build/p/p
echo '       ++ " edited"' >> ../q/Q.hs
cabal build > /dev/null
./dist/build/p/p
