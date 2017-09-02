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

    # Check if program completed successfully...
    if [ $? -eq 0 ]; then

      # Check output differences
      diff tmp.out "$filename.good" > "$filename.diff"
      diffLen="$(wc -c $filename.diff | awk '{print $1}')"

      # Output differences if incorrect...
      if [ $diffLen -eq 0 ]; then
        echo "[$filename]: SUCCESS"
      else
        echo "[$filename]: OUTPUT-DIFF"
        cat "$filename.diff"
      fi

      rm "$filename.diff"

    else
      echo "[$filename]: RUNTIME ERROR"
      cat tmp.err
    fi

    # Cleanup temporaries
    rm "$filename.class"
    rm tmp.out
    rm tmp.err

  done

  cd ../
done

cd ../
