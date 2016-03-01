. ../common.sh
cd package
cabal sandbox init
cabal sandbox add-source ../dependency-1
cabal sandbox add-source ../dependency-2
echo
echo 1st install ____________________________________________________________
echo
cabal install -v3
echo
echo 2nd install ____________________________________________________________
echo
cabal install -v3
