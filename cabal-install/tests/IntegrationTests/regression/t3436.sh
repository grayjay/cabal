. ./common.sh
cd t3436
cabal sandbox init
cabal install ./Cabal-99998
cabal sandbox add-source Cabal-99999
! cabal install 2>&1 > error
cat error
cat error | grep -q "This is Cabal-99999" \
    || die "Expected output from Cabal-99999."
