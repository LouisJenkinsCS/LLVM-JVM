#!/bin/bash

# Rebuild 'Main' in case it has been changed...
ghc Main.hs
if [ $? -eq 0 ]; then
  echo Main.exe Rebuilt
else
  echo Main.exe Failed
  exit
fi

# All files should be in the 'Tests' directory
cd Unit

# Search all directories
for d in */; do
  cd $d
  echo "cd $d"

  # We only search for files with the 'java' extension
  for f in *.java; do
    filename=$(basename "${f%.*}")
    javac $f
    ../../Main.exe "$filename.class" 1> tmp.out 2> tmp.err
    if [ $? -eq 0 ]; then
      echo "[$filename]: SUCCESS"
    else
      echo "[$filename]: FAILURE"
      cat tmp.err
    fi

    rm "$filename.class"
    rm tmp.out
    rm tmp.err

  done

  cd ../
done

cd ../
