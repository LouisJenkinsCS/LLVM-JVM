##################################
##        Instructions:
##################################

Run the 'Main.hs' after installing (for installation see below), and the menu should allow you to choose which files to run. This program contains with it two equivalent test files for two langauges. The test files are very simple, but then again, so is this virtual machine. You can toggle 'Debug' on by choosing 'Toggle Debug' as it is primarily off: Be warned, however, that the debugging information is meant to be displayed from my own programming environment (Atom text editor) and may not look too appealing, but it isn't really meant to be.

Theoretically, any language which produces byte code for the JVM will work, but insofar, Java and Scala are the only two proven to work, and Scala requires a bit of extra work: Scala will produce two '.class' files, one that's [CLASS_NAME].class, and [CLASS_NAME]$.class. The former I delete, and the latter I rename to the former. It seems that Scala generates some bootstrap code to startup the Scale runtime environment, and then invokes main, but that's not useful for my virtual machine, but it wouldn't compile correctly either, so I just get rid of it. The file ending with '$.class' however seems almost bytecode-for-bytecode, with exception to the constant pool, but thankfully my implementation is general enough to step around those differences.

It is recommended that you use some kind of text editor with syntax highlighting. I use 'Atom', but 'Visual Studio Code' and 'Sublime' should suffice.

##################################
##	  Installation
##################################

To Install: 'cabal build && cabal install'
To run (from root of project): './dist/build/Minimal-JVM/Minimal-JVM.exe'

Notes: You should run the executable from the root directory, either by moving it there or invoking it via commandline like the above. Also as well, note that if you choose 'Custom' from the menu, the '.class' file SHOULD not contain too comlex code. What constitutes as 'too complex' varies from compiler-to-compiler and language-to-language.

Documentation is rather premature, but you can open it by viewing 'Documentation/index.html'
