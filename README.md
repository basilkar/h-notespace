# h-notespace

Sample features:

1. input a set of notes, output fitting scales {haskell-prototyping.hs: improScale}
2. input a scale, output its chords {haskell-prototyping.hs: scalechords}
3. recognize known chords and scales {haskell-prototyping.hs: ictRecognizer}
4. print all seventh chords within a given scale
5. input a melody, output a harmonization
6. input keys X, Y and an n, output n keys on a tonnetz/circle-of-fifths/... geodesic of X & Y
7. input a chord progression, output its negative (in the sense of Ernst Levy)
8. run fretboard quizzes {haskell-prototyping.hs: fretboardQuizOneStandard, fretboardQuizTwoStandard}

## Compilation

Working in `stack`, you should just  
`~/h-notespace$ stack build`  
and then  
`~/h-notespace$ stack exec h-notespace-exe`

## Contents of the old file `haskell-prototyping.hs`

0. Preamble and stolen stuff
1. An enumeration type for notes  
   1.1. Application: Impro suggester  
   1.2. Application: Scale chords  
   1.3. Application: Interval-chord-scale recognizer  
   1.4. Application: The seventh chords of a scale  
   1.5. Application: A literate suggester
2. An inductive type for chord constructions
3. About rhythm
4. Tymoczko chord change spaces  
   4.1. T-Closeness
5. Concerning the guitar  
   5.1. Application: Fretboard quizes

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag h-notespace:library-only --flag h-notespace:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
