##################################
##        Instructions:
##################################

Main.hs can be modified to run the '.class' files specified in the Examples folder.
To include your own, you must compile it with your JVM compiler of choice. Currently, either Scala
or Java will work (Scala must use the '*$.class' file, meaning the identical class file with the same
file name but with a dollar sign as a suffix). Theoretically, this should work with any JVM language
which only has basic operations that do not invoke the Runtime, which makes this Virtual Machine all the
more fun.

It is recommended that you use some kind of text editor with syntax highlighting. I use 'Atom', but 'Visual Studio Code' and 'Sublime' should suffice.

To Install: 'cabal build && cabal install'
To run (from root of project): './dist/build/Minimal-JVM/Minimal-JVM.exe'

Notes: This is a very early build of the program, and as such, isn't really flexible.
It requires that the program be called from the directory in which the test file is
located in 'Examples' subdirectory. It should be fixed soon, but you never know what might come up.

Documentation is rather premature, but you can open it by viewing 'Documentation/index.html'
