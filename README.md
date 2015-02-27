Pict3D: A 3D engine with a purely functional API
================================================

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

Eventually, there will be documentation. For now, use the tests as examples or read through the (as-yet incomplete) tutorial.

To read through all the tutorials, run the program

```racket
#lang racket
(require pict3d/tutorials)
```

To read through just one, run a program like

```racket
#lang racket
(require pict3d/tutorial/00)
```

To open the test files, in DrRacket, click the `File -> Open Require Path...` menu item. Type `pict3d/tests`, click `Enter subcollection`, choose a Racket source file, and click `OK`.
