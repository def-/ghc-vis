Visualize live Haskell data structures in GHCi
----------------------------------------------

ghc-vis is a tool to visualize live Haskell data structures in GHCi. Evaluation is not forced and you can interact with the visualized data structures. This allows seeing Haskell’s lazy evaluation and sharing in action.

Author: Dennis Felsing <dennis@felsing.org>

Contributions, suggestions, and bug reports are welcome!

# Documentation

At [https://dennis.felsing.org/ghc-vis](https://dennis.felsing.org/ghc-vis)

# Installation

## Windows and Stack

1) Once

```sh
$ stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-gtk3 mingw-w64-x86_64-gtk2 wget unzip 
$ stack exec -- echo 'export PATH=/c/graphviz/bin:$PATH' >> ~/.bashrc
$ stack install gtk2hs-buildtools glib cairo pango gtk
$ git clone https://github.com/def-/ghc-vis.git
$ stack install
```

2) Each time

In ghc-vis folder:

```she
$ stack repl
ghci> :script ghci
ghci> :vis
```

Installation can be tricky, if you run into problems you can check [Ou42's successful installation](https://gist.github.com/Ou42/b34daba4b7183a2bf79cabea37647855).
