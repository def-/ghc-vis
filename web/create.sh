#!/usr/bin/env zsh

insert () {
  width=$(file $1.png | cut -d" " -f5)
  height=$(file $1.png | cut -d" " -f7 | cut -d"," -f1)
  cat <<EOF
<img alt="Demo" width="${width}" height="${height}" src="${1}.svgz" />
EOF
}

ghci -ghci-script create.ghci

for i in *svg; do
  inkscape -z -e ${i:r}.png -d 60 -b white $i

  gzip -9 $i
  mv $i.gz ${i:r}.svgz

  pngnq ${i:r}.png
  optipng -o7 ${i:r}-nq8.png
  mv ${i:r}-nq8.png ${i:r}.png
done

cat > index.html <<EOF
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
  <head>
    <meta http-equiv="Content-Type" content="text/html;charset=utf-8" />
    <meta http-equiv="Content-Language" content="en_US" />
    <link rel="stylesheet" type="text/css" href="../css.css" />
    <title>ghc-vis: Live visualization of data structures in GHCi</title>
  </head>
  <body>
    <h1>ghc-vis</h1>
    <p>
      ghc-vis is a tool to visualize live Haskell data structures in GHCi.
      Evaluation is not forced and you can interact with the visualized data
      structures. This allows seeing Haskell's lazy evaluation and sharing in
      action.
    </p>
    <ul>
      <li><a href="#introduction">Introduction</a></li>
      <li><a href="#basic-usage">Basic Usage</a></li>
      <li><a href="#other-tools">Other Tools</a></li>
      <li><a href="#more-examples">More Examples</a></li>
      <li><a href="#combined-debugger">Combined with GHCi's Debugger (Video example)</a></li>
      <li><a href="#symbols-commands">Symbol Explanations and Commands</a></li>
      <li><a href="#key-bindings">Key and Mouse Bindings</a></li>
      <li><a href="#as-library">Using ghc-vis as a Library</a></li>
      <li><a href="#thesis">Thesis</a></li>
      <li><a href="#issues">Bugs, Feature Requests, Development</a></li>
    </ul>
    <h2 id="introduction">Introduction</h2>
    <p>
      Functional programming languages like Haskell offer a way to write
      well-structured software, which is easy to understand and still performs
      well. In order to achieve this one has to be able to consciously use
      features that result from the paradigm of functional programming.
    </p><p>
      ghc-vis is a tool for visualizing two features of this kind, namely lazy
      evaluation and sharing, in a way that can be used in the (self-)teaching
      of Haskell and the development and debugging of Haskell software.
    </p><p>
      A common example for the power of lazy evaluation are infinite data
      structures. They are easy to define and convenient to use in Haskell. We
      can write
    </p>
    <pre class="code">
ones = [1,1..]</pre>
    <p>
      to construct an infinite list of 1s, which we can now access by the
      suggestive name <em>ones</em>. How does this work? Clearly the entire
      list can’t be stored in memory as we do not possess the infinite tape of
      a turing machine.
    </p><p>
      We can write a function to retrieve the nth member of a list:
    </p>
    <pre class="code">
at 0 (x:xs) = x
at n (x:xs) = at (n-1) xs</pre>
    <p>
      Evaluating <em>at 1 ones</em> extracts the second member of the list
      <em>ones</em>, which gives us the integer 1. In the same manner <em>at 2
      ones</em> and <em>at 3 ones</em> give us 1. But what is happening to the
      infinite list of ones as we access its members?
    </p>
    <div class="subfig">
      <p>After <em>at 1 ones:</em></p>
      $(insert intro1-1)
    </div>
    <div class="subfig">
      <p>After <em>at 2 ones:</em></p>
      $(insert intro1-2)
    </div>
    <div class="subfig">
      <p>After <em>at 3 ones:</em></p>
      $(insert intro1-3)
    </div>
    <p class="subfig-after">
      The solution is that the infinite list of ones gets constructed just as
      we access it. Thinking about this leads us to a problem: When we evaluate
      <em>at (10 ^ 7) ones</em> a huge list gets constructed and a lot of
      memory is used to store it. There must be a way to avoid this and indeed
      there is. We define the infinite list of ones in a different way:
    </p>
    <pre class="code">
ones' = 1 : ones'</pre>
    <p>
      The resulting list is the same and we can access it in the same way. But
      when we evaluate <em>at (10 ^ 7) ones'</em> memory usage does not seem to
      increase at all. What is the reason?
    </p>
    <div class="subfig">
      <p>After <em>at 1 ones':</em></p>
      $(insert intro2)
    </div>
    <div class="subfig">
      <p>After <em>at 2 ones':</em></p>
      $(insert intro2)
    </div>
    <div class="subfig">
      <p>After <em>at 3 ones':</em></p>
      $(insert intro2)
    </div>
    <p class="subfig-after">
      This time no big list is constructed in memory, instead a list is created
      that references itself as the rest of the list.
    </p>
    <h2 id="basic-usage">Basic usage</h2>
    <p>
      The <a href="http://hackage.haskell.org/package/ghc-vis">package</a> is
      available on Hackage. Install it like this:
    </p>
    <pre class="code">
$ cabal install ghc-vis
$ echo ":script $HOME/.cabal/share/ghc-vis-0.7/ghci" >> ~/.ghci</pre>
    <p>
      You need GTK, Cairo, Pango, Graphviz and their corresponding Haskell
      library bindings. If you run into any problems, try this:
    </p>
    <pre class="code">
$ cabal update
$ cabal install gtk2hs-buildtools</pre>
    <p>
      Now you can run ghci and experiment with ghc-vis. Start the
      visualization:
    </p>
    <pre class="code">
$ ghci
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
λ> :vis</pre>
    <p>
      A window should appear now. This is the visualization window:<br/>
      <img alt="Demo" width="416" height="379" src="window.png" /><br/>
      Add an expression to the visualization:
    </p>
    <pre class="code">
λ> let a = [1..3]
λ> :view a
λ> let b = cycle a
λ> :view b
λ> :view "foo" ++ "bar"</pre>
    <p>
      You should now see something similar to this:<br/>
      $(insert 1)
    </p>
    <p>
      Thunks are red, Functions are yellow, named objects are green and links
      to an already shown object are blue.
    </p>
    <p>
      Notice how a is referenced by b.
    </p>
    <p>
      Evaluate an object that is shown in the visualization. You can also click
      on the object to evaluate it. Only thunks, which are named starting with
      a t, can be evaluated.
    </p>
    <pre class="code">λ> :eval t2</pre>
    <p>
      $(insert 2)
    </p>
    <p>
      The first element of b has been evaluated. We see that it's a reference
      to the value that's also referenced in a, as they share the same name
      "b0". "S# 1" stands for the Integer data constructor, which is called S#
      in GHC and its argument 1.
    </p>
    <p>
      Switch between the list view and the graph view:
    </p>
    <pre class="code">λ> :switch</pre>
    <p>
      $(insert 3)
    </p>
    <p>
      When an object is updated by accessing it, you have to call :update to
      refresh the visualization window. You can also click on an object to
      force an update:
    </p>
    <pre class="code">
λ> a !! 2
3
λ> :update</pre>
    <p>
      $(insert 4)
    </p>
    <p>
      Clear the visualization window, this also happens when you :load or
      :reload a source file:
    </p>
    <pre class="code">
λ> :clear</pre>
  <h2 id="other-tools">Other Tools</h2>
    <p>
      The GHCi Debugger has a :print command that inspects data structures at
      runtime. Evaluation is not forced, so there are no side effects.
    </p>
    <pre class="code">
$ ghci
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
λ> let a = [1..3]
λ> :print a
a = (_t1::[Integer])
λ> head a
1
λ> :print a
a = 1 : (_t2::[Integer])
λ> head $ tail a
2
λ> :print a
a = 1 : 2 : (_t3::[Integer])
λ> a
[1,2,3]
λ> :print a
a = [1,2,3]
λ> let b = a ++ a
λ> head b
1
λ> :print b
b = 1 : (_t3::[Integer])
λ> b
[1,2,3,1,2,3]
λ> :print b
b = [1,2,3,1,2,3]</pre>
    <p>
      These data structures reside on the GHC heap. We can see that expressions
      are only evaluated once they are needed. This is called lazy evaluation.
      To avoid unnecessary copies of objects on the heap a heap object can be
      referenced multiple times instead of being copied into a new location.
      This is called sharing. We can see that :print does not tell us how b is
      sharing values with a or within itself.
    </p>
    <p>
      <a
      href="http://hackage.haskell.org/package/vacuum-cairo">Vacuum-cairo</a>
      on the other hand does tell us how values are shared within b:
    </p>
    <pre class="code">λ> System.Vacuum.Cairo.view b</pre>
    <p>
      The following window appears on the screen:<br/>
      <img alt="Demo" src="vacuum1.png"/>
    </p>
    <p>
      But vacuum-cairo evaluates the data structure fully before showing it. So
      we can't use it to see how data structures are lazily evaluated in
      Haskell.
    </p>
    <p>
      Additionally Vacuum-cairo is unable to visualize sharing between
      different data structures, like a and b in this case, as only one data
      structure can be viewed at any time. We would have to combine the data
      structures into one to see which values are referenced in both.
    </p>
    <p>
      When we use <a
      href="http://hackage.haskell.org/package/vacuum">vacuum</a> (and <a
      href="http://hackage.haskell.org/package/vacuum-graphviz">vacuum-graphviz</a>)
      directly we can inspect data structures without evaluating them:
    </p>
    <pre class="code">
λ> let a = "foo"
λ> let b = a ++ a
λ> head b
'f'
λ> GHC.Vacuum.GraphViz.graphToDotFile "vacuum2" Data.GraphViz.Commands.Png $
   GHC.Vacuum.nameGraph (GHC.Vacuum.vacuumLazy (a,b))
    </pre>
    <p><img alt="Demo" src="vacuum2.png"/></p>
    <p>
      We can see that the value "C#l3" is referenced both in a and b. That's
      the 'f'. What we can't see is that the l5 thunk references a and has
      another value shared with a.
    </p>
    <p>
      What we'd like to see is something like this, which is what ghc-vis
      outputs:
    </p>
    <p>
      $(insert 0)
    </p>
    <h2 id="more-examples">More Examples</h2>
    <p id="fibonacci">
      Let's consider a function f that calculates fibonacci numbers:
    </p>
    <pre class="code">
λ> let f = 0 : 1 : zipWith (+) f (tail f)
λ> :switch
λ> f !! 2
1
λ> :view f
λ> f !! 3
1
λ> :update
λ> f !! 4
2
λ> :update</pre>
    <div class="subfig">
      <p>After <em>f !! 2:</em></p>
      $(insert fib1)
    </div>
    <div class="subfig">
      <p>After <em>f !! 3:</em></p>
      $(insert fib2)
    </div>
    <div class="subfig">
      <p>After <em>f !! 4:</em></p>
      $(insert fib3)
    </div>
    <p class="subfig-after">
      Interpreted and compiled code may looks very different when evaluating.
      First an example of interpreted code, -fbyte-code is the default in
      GHCi if the code hasn't been compiled already:
    </p>
    <pre class="code">
λ> :set -fbyte-code
λ> :!cat Sieve.hs
module Sieve where
primes = sieve [2..] where sieve (p:xs) = p : sieve [x | x &lt;- xs, x \`mod\` p &gt; 0]
λ> :l Sieve
[1 of 1] Compiling Sieve            ( Sieve.hs, interpreted )
Ok, modules loaded: Sieve.
λ> head primes
2
λ> :switch
λ> :view primes</pre>
    <p>
      $(insert interpreted)
    </p>
    <p>
      The view is rather confusing as a lot of type class information is
      included. Let's look at compiled object code:
    </p>
    <pre class="code">
λ> :set -fobject-code
λ> :l Sieve
[1 of 1] Compiling Sieve            ( Sieve.hs, Sieve.o )
Ok, modules loaded: Sieve.
λ> head primes
2
λ> :switch
λ> :view primes
    </pre>
    <p>
      $(insert compiled)
    </p>
    <pre class="code">
λ> let l = [1,2,3]
λ> :view l
λ> let l2 = 4:l
λ> :view l2
λ> let x = l ++ l2
λ> :view x
λ> let y = id (:) () y
λ> :view y
λ> :eval t1
λ> :switch</pre>
    <p>
      $(insert 5)
      $(insert 6)
    </p>
    <pre class="code">
λ> data BinaryTree = BT BinaryTree Int BinaryTree | Leaf
λ> let x = BT (BT (BT Leaf 1 (BT Leaf 2 Leaf)) 3 (BT (BT Leaf 4 (BT Leaf 5 Leaf)) 6 Leaf)) 7 Leaf
λ> :view x
λ> :switch</pre>
    <p>
      $(insert bt)
    </p>
    <p>A working cyclic double linked list: <a href="dll.hs">dll.hs</a></p>
    <pre class="code">
λ> :l dll.hs
λ> let x = mkDList [1..4]
λ> :view x
λ> :switch</pre>
    <p>
      $(insert dll)
    </p>
    <p>
      A non-working cyclic double linked list: <a href="dll2.hs">dll2.hs</a>
      New list elements get created all the time instead of referencing the
      existing ones.
    </p>
    <pre class="code">
λ> :l dll2.hs
λ> let x = mkDList [1..4]
λ> :view x
λ> :switch</pre>
    <p>
      $(insert dll2)
    </p>
    <p><a href="dfa.hs">dfa.hs</a></p>
    <pre class="code">
λ> :l dfa
λ> :view dom18
λ> :switch</pre>
    <p>
      $(insert dfa)
    </p>
    <pre class="code">
λ> :view (Data.IntMap.fromList $ zip [1..10] [1..])
λ> :eval t0
λ> :switch</pre>
    <p>
      $(insert intmap)
    </p>
    <pre class="code">
λ> let a = Data.ByteString.pack [0x44,0x45,0x46]
λ> :view a
λ> let b = Data.ByteString.append a a
λ> :view b
λ> :switch
λ> a
"DEF"
λ> :update</pre>
    <p>
      $(insert bytestring)
    </p>
    <pre class="code">
λ> let b = GHC.Arr.array ((1,1),(3,2)) [((1,1),42),((1,2),23),((2,1),999),((2,2),1000),((3,1),1001),((3,2),1002)]
λ> b
array ((1,1),(3,2)) [((1,1),42),((1,2),23),((2,1),999),((2,2),1000),((3,1),1001),((3,2),1002)]
λ> :view b
λ> :switch</pre>
    <p>
      $(insert array)
    </p>
    <h2 id="combined-debugger">Combined with GHCi's Debugger</h2>
    <p>
      ghc-vis can be used to watch data structures while a computation is
      inspected using GHCi's debugger:
    </p>
    <video width="640" height="480" controls>
      <source src="debugger.mp4" type="video/mp4" />
      <source src="debugger.webm" type="video/webm" />
      Your browser does not seem to support h264 videos, please down the video below.
    </video>
    <p><strong>Download Video:</strong> <a href="debugger.mp4">debugger.mp4</a>, <a href="debugger.webm">debugger.webm</a></p>
  <h2 id="symbols-commands">Symbol Explanations and Commands</h2>
    <p>
      These objects are used in GHCi:
    </p>
    <table>
      <tr class="header">
        <th>Name</th><th>Graph View</th><th>List View</th><th>Explanation</th>
      </tr><tr>
        <td>Thunk</td><td>Thunk</td><td>t0</td><td>Unevaluated value</td>
      </tr><tr>
        <td>General Application</td><td>AP</td><td>t0</td><td>Unevaluated value (mostly used in interpreted code)</td>
      </tr><tr>
        <td>Function</td><td>Fun</td><td>f0</td><td>Function applied to less parameters than it takes (possibly 0)</td>
      </tr><tr>
        <td>Partial Application</td><td>PAP</td><td>f0</td><td>Function applied to less parameters than it takes (mostly used in interpreted code)</td>
      </tr><tr>
      </tr>
    </table>
    <p>
      Commands are executed on the GHCi command line:
    </p>
    <table>
      <tr class="header">
        <th>Command</th><th>Action</th>
      </tr><tr>
        <td>:vis</td><td>Run the visualization window</td>
      </tr><tr>
        <td>:mvis</td><td>Run the minimal visualization window</td>
      </tr><tr>
        <td>:view x</td><td>Add the Haskell expression <em>x</em> to the visualization window</td>
      </tr><tr>
        <td>:eval t</td><td>Force evaluation to WHNF of the thunk named <em>t</em></td>
      </tr><tr>
        <td>:switch</td><td>Switch between the visualization types (list, graph)</td>
      </tr><tr>
        <td>:update</td><td>Update the visualization window, in case an expression has changed</td>
      </tr><tr>
        <td>:clear</td><td>Remove all visualized expressions</td>
      </tr><tr>
        <td>:restore</td><td>Restore all hidden nodes</td>
      </tr><tr>
        <td>:timeback</td><td>Go back in history</td>
      </tr><tr>
        <td>:timeforward</td><td>Go forward in history</td>
      </tr><tr>
        <td>:export file</td><td>Export the current visualization to a file; <em>SVG, PDF, PS</em> and <em>PNG</em> supported</td>
      </tr>
    </table>
  <h2 id="key-bindings">Key and Mouse Bindings</h2>
    <p>
      Key and mouse bindings work in the visualization window:
    </p>
    <table>
      <tr class="header">
        <th colspan="3">Keys</th><th>Mouse</th><th>Action</th>
      </tr><tr>
        <td class="cont">v</td><td colspan="3"></td><td>Switch view</td>
      </tr><tr>
        <td class="cont">c</td><td colspan="3"></td><td>Clear view</td>
      </tr><tr>
        <td class="cont">C</td><td colspan="3"></td><td>Restore hidden nodes</td>
      </tr><tr>
        <td class="cont">u</td><td colspan="3"></td><td>Update view</td>
      </tr><tr>
        <td class="cont">Enter, </td><td colspan="2">Space,</td><td>Left click</td><td>Evaluate and update</td>
      </tr><tr>
        <td class="cont">,</td><td colspan="3"></td><td>Go back in history</td>
      </tr><tr>
        <td class="cont">.</td><td colspan="3"></td><td>Go forward in history</td>
      </tr><tr>
        <td class="cont"></td><td class="cont"></td><td></td><td>Right click and drag</td><td>Move</td>
      </tr><tr>
        <td class="cont">Left, </td><td class="cont">h, </td><td>a</td><td></td><td>Move left</td>
      </tr><tr>
        <td class="cont"></td><td class="cont">H, </td><td>A</td><td></td><td>Move left fast</td>
      </tr><tr>
        <td class="cont">Right, </td><td class="cont">l, </td><td>d</td><td></td><td>Move right</td>
      </tr><tr>
        <td class="cont"></td><td class="cont">L, </td><td>D</td><td></td><td>Move right fast</td>
      </tr><tr>
        <td class="cont">Up, </td><td class="cont">k, </td><td>w</td><td></td><td>Move up</td>
      </tr><tr>
        <td class="cont"></td><td class="cont">K, </td><td>W</td><td></td><td>Move up fast</td>
      </tr><tr>
        <td class="cont">Down, </td><td class="cont">j, </td><td>s</td><td></td><td>Move down</td>
      </tr><tr>
        <td class="cont"></td><td class="cont">J, </td><td>S</td><td></td><td>Move down fast</td>
      </tr><tr>
        <td class="cont">+, </td><td colspan="2">PageUp</td><td>Wheel up</td><td>Zoom in</td>
      </tr><tr>
        <td class="cont">-, </td><td colspan="2">PageDown</td><td>Wheel down</td><td>Zoom out</td>
      </tr><tr>
        <td class="cont">0, </td><td colspan="2">=</td><td>Wheel click</td><td>Reset zoom and position</td>
      </tr>
    </table>
  <h2 id="as-library">Using ghc-vis as a Library</h2>
    <p>
      Although ghc-vis is meant to be used in GHCi it can also be used as a
      library in regular Haskell programs which are run or compiled by GHC. You
      can run those programs using "runghc example.hs" or "ghc -threaded
      example.hs &amp;&amp; ./example". Without the "-threaded"-Flag ghc-vis
      does not work correctly. This is an example using ghc-vis outside of
      GHCi:
    </p>
    <pre class="code">
import GHC.Vis

main = do
  let a = "teeest"
  let b = [1..3]
  let c = b ++ b
  let d = [1..]
  putStrLn $ show $ d !! 1

  visualization
  view a "a"
  view b "b"
  view c "c"
  view d "d"

  getChar
  switch

  getChar</pre>
  <h2 id="thesis">Thesis</h2>
    <p>
      ghc-vis was developed as part of my Bachelor Thesis titled <a
      href="thesis">Visualization of Lazy Evaluation and Sharing</a>. It
      contains some more examples, explains how it works and what kind of
      problems I encountered.
    </p>
  <h2 id="issues">Bugs, Feature Requests, Development</h2>
    <p>
      If you have any problems, new ideas or comments concerning ghc-vis, just
      drop me an email: <a
      href="mailto:dennis@felsin9.de">dennis@felsin9.de</a>. I'll be glad to help you.
    </p>
  </body>
</html>
EOF
