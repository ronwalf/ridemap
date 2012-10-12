# ridemap

Creates a [combined map](http://www.cs.umd.edu/~ronwalf/2012/map/) of Golden Cheetah activities suitable for embedding on a web page.


## Requirements and installation.
Ridemap requires a recent copy of [Haskell](http://hackage.haskell.org/platform/), being most recently tested on Haskell Platform 2012.2.0.0, as well as a number of other libraries that are managed via [Cabal](http://www.haskell.org/cabal/).
Once Haskell is installed, use cabal to build and install ridemap:

    $ cd ridemap
    $ cabal install --user

This installs one executable, `ridemap`, which takes a path to a directory and any number of [Golden Cheetah](http://goldencheetah.org/) JSON ride files on the command line, and outputs to that directory a JSON data structure to be loaded by the Javascript in the `html` directory.  Here is a minimal example that should produce a map:

    $ cd ridemap
    $ ridemap html/data /path/to/GC/rides/\*.json 
    $ scp -Cr html/ remote.host:/path/to/map/dir/

To view the map, it must be through a web server, since browsers restrict XMLHttpRequest for local files.


There are three relevant command line options:
    
    -r NUM : this specifies the radius of the hexagonal tiles in terms of meters at the equator (the projection distorts this away on points away from the equator.
    -s NUM : The minimum time step in seconds when interpolating between ride points
    -m NUM : The maximum time gap in seconds between ride points before that `ridemap` will attempt to interpolate between.
    -g : Use the Google/Bing mercator projection, not WGS 84.


 
