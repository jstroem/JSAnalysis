#!/bin/bash
# Check if we received the correct # of args
if [ $# -ne 1 ]
then
  echo "Usage: `basename $0` {test case directory}"
  exit -1
fi

DIR=$1
# Check if the argument specifies an existing directory
if [ ! -d "$DIR" ]; then
    echo "$DIR is not a valid directory. Exiting"
    exit -2
fi

# Make a directory to hold the output if it doesn't exist
OUTDIR="$DIR/ast"
if [ ! -d "$OUTDIR" ]
then
    mkdir -p "$OUTDIR"
fi

FILES="$DIR/*.js"
for f in $FILES
do
  echo "Processing $f"
  filename=$(basename "$f")
  filename="${filename%.*}"
  scala JSAnalyzer.Test $f > "$OUTDIR/$filename.dot"
  dot "$OUTDIR/$filename.dot" -Tpng > "$OUTDIR/$filename.png"
done
