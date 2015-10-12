Waxeye Parser Generator
=======================

Waxeye is a parser generator based on parsing expression grammars (PEGs). It
supports C, Java, Javascript, Python, Ruby and Scheme.


Features
--------

* Choice of Programming Language
  - C
  - Java
  - Javascript
  - Python
  - Ruby
  - Scheme

* Scanner-less Parsing

* Automatic AST Generation

* Language Independent, Reusable Grammars

* Modular, Composable Grammars

* Grammar Testing


User Manual
-----------

Waxeye's user manual is in `docs/manual.html`. The latest version is also
online at http://waxeye.org/manual.html.


Installation
------------

### Unix and OSX

1. Extract the files of the distribution.

2. Copy the `waxeye` directory to where you wish to install it.

3. Add the `bin/waxeye` binary to your search path. e.g. If you have `~/bin` in
   your `PATH` and installed waxeye to `/usr/local/waxeye` then you might do
   the following.

   `ln -s /usr/local/waxeye/bin/waxeye ~/bin/`


### Windows

1. Extract the files of the distribution.

2. Copy the `waxeye` directory to where you wish to install it.


Running
-------

### Unix and OSX

Use the `waxeye` command.

### Windows

Use a command prompt to run `waxeye.exe`. Note: If using the interpreter under
Windows, you will need to press `Ctrl-z` and then 'Enter' after the input you
want to interpret.


Building from Source
--------------------

1. Install [Racket](http://racket-lang.org)

2. Install Waxeye's backend for Scheme.
   * Unix and OSX

     `sudo ln -s /usr/local/waxeye/src/scheme/waxeye /usr/local/racket/lib/racket/collects/`

   * Windows

     Copy the directory `src/scheme/waxeye` into your Racket `collects`
     directory. For example, `C:\Program Files\Racket\collects`.

3. Build Waxeye
   * Unix and OSX

     `./build/unix`

   * Windows

     - If your Racket installation isn't `C:\Program Files\Racket`, then you
       will need to modify `build\exe.bat` to use the correct path.

     - From your Waxeye installation directory, run the `build\exe.bat` script
       in a command prompt.

Running tests
-------------

* To run JavaScript tests: `node test/javascript/test.js`

* To run Java tests:
   1. You should download and compile `jcommander` and `TestNG` and export them:
   2. `export CLASSPATH=~/Downloads/jcommander-1.48.jar; export TESTNG_LIB=~/testng/target/testng-6.9.9-SNAPSHOT.jar`
   2. `ant -f build/ant.xml clean init testng`

Support
-------

* [Mailing List](https://lists.sourceforge.net/lists/listinfo/waxeye-users)

* [Issue Tracker](https://github.com/orlandohill/waxeye/issues)


License
-------

MIT -- All files (except the user manual) are under the permissive MIT license.

GNU FDL -- Waxeye's user manual is under the GNU Free Documentation License.
This includes the files `doc/book/book` and `doc/manual.html`.
