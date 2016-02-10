. ../common.sh

cabal sandbox delete
cabal exec my-executable && die "Unexpectedly found executable"

cabal sandbox init
cabal install

# Execute indirectly via bash to ensure that we go through $PATH
cabal exec sh -- -c my-executable || die "Did not find executable"
