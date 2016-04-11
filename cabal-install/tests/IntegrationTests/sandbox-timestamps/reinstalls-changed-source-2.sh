. ./common.sh

cd p
cabal sandbox init > /dev/null

cabal sandbox add-source ../q > /dev/null

cabal install --only-dependencies > /dev/null
cabal run p -v0
echo '       ++ " edited"' >> ../q/Q.hs
cabal run p -v0
