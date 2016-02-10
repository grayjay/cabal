. ../common.sh

cabal sandbox delete
cabal exec my-executable && die "Unexpectedly found executable"

cabal sandbox init
cabal install

cabal exec my-executable || die "Did not find executable"
