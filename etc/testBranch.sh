#!/bin/bash
set -e
set -u

testBranch=$1;

gitrepo="$(git rev-parse --show-toplevel)"

masterdir=/tmp/hchess-master
newdir="/tmp/hchess-$testBranch"

rm -rf /tmp/hchess-master;
rm -rf /tmp/hchess-$testBranch;

git clone "$gitrepo" "$masterdir"
git clone "$gitrepo" "$newdir";

pushd "$masterdir"
git checkout master
popd

pushd "$newdir"
git checkout "$testBranch"
popd

for dir in "$masterdir" "$newdir"; do
    pushd "$dir";
    cabal build
    exe=$(find dist-newstyle -executable -name hchess -type f)
done

cutechess-cli \
    -engine name=hchess-new proto=uci  cmd="$newdir/$exe" \
    -engine name=hchess-master proto=uci cmd="$masterdir/$exe" \
    -openings file="$masterdir"/etc/silversuite.pgn \
    -concurrency 3 -ratinginterval 10 -games 500 -pgnout games.pgn \
    -each  tc=5+0.01 -repeat
