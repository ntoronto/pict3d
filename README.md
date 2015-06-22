Pict3D: A 3D engine with a purely functional API
================================================

Browse [the documentation](http://pkg-build.racket-lang.org/doc/pict3d/index.html).

To use:

 1. Install Racket from http://download.racket-lang.org/.
 2. Open DrRacket.
 3. Open the `File` menu and choose `Install Package...`.
 4. Type `pict3d` in the "Package Source" field and click `Install`.
 5. Wait for a few minutes.
 6. Test by running a program like the following.

```racket
#lang racket
(require pict3d)
(sphere origin 1/2)
```

Press F1 in DrRacket and search for "pict3d" to find the documentation.

If you'd like to browse through the test programs, in DrRacket, click the `File -> Open Require Path...` menu item. Type `pict3d/tests`, click `Enter subcollection`, choose a Racket source file, and click `OK`. You'll have to either uncheck `Populate compiled directories` in the language dialog (`Ctrl-L` or `Langauge -> Choose Language...`) or copy the text of the test files into a fresh buffer to run them.
