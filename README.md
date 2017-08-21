Waxeye Parser Generator [![Build Status][badge-travis]][travis]
===============================================================

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

2. Build Waxeye
   * Unix and OSX

     `./build/unix`

   * Windows

     - If your Racket installation isn't `C:\Program Files\Racket`, then you
       will need to modify `build\exe.bat` to use the correct path.

     - Run the `build\exe.bat` script. The `waxeye.exe` executable
       will be saved to the directory you run the script from.

Running tests
-------------

First, install all packages necessary for running the tests.
On Ubuntu, run:

```bash
sudo apt-get install racket nodejs ant checkstyle testng
```

To then run all the tests, run:

```bash
test/bin/test-all
```

To run individual language tests, run the respective script, e.g. for JavaScript:

```bash
test/bin/test-javascript
```

Support
-------

* [Mailing List](https://lists.sourceforge.net/lists/listinfo/waxeye-users)

* [Issue Tracker](https://github.com/orlandohill/waxeye/issues)


License
-------

MIT -- All files (except the user manual) are under the permissive MIT license.

GNU FDL -- Waxeye's user manual is under the GNU Free Documentation License.
This includes the files `doc/book/book` and `doc/manual.html`.

[badge-travis]: https://img.shields.io/travis/orlandohill/waxeye.svg
[travis]: https://travis-ci.org/orlandohill/waxeye
