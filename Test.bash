#!/bin/bash

# Rebuild 'Main' in case it has been changed...
ghc Main.hs -o Main.exe
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
  # Note that currently we can only run Java 7 '.class'
  # files, so we must cross-compile it...
  for f in *.java; do
    filename=$(basename "${f%.*}")

    # Skip
    if [ -f "$filename.skip" ]; then
      echo "Skipping $filename"
      continue
    fi

    bootstrap=$(locate -r '/rt.jar$')
    javac -target 1.7 -source 1.7 -bootclasspath $bootstrap $f
    ../../Main.exe -cp ../../rt:./ "$filename" 1> tmp.out 2> tmp.err

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
