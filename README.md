For solving the AoC2016 advent calendar. http://adventofcode.com/2016

# Instructions

I stole much of my directory structure from (Tomash Aschan)[https://github.com/tomasaschan/advent-of-code-2018].
The migration to this structure is __in progress_ so only day 1 is in this structure, and the others are standalones that should be run interactively in their respective folders.

To just run a single day, install libraries with `stack setup`, compile all code with `stack build` and then run `stack exec aoc2016 -- <dayNr>` for day `<dayNr>`.

I ran `stack build --test --fast --file-watch --exec "aoc2016 <dayNr>` where `<dayNr>` is the day, to run test suites and my current day.

## Alternative running
Another way to run the code is with `ghcid`. I find it REALLY nice for these small things. 
To run Day17 upon saves, and revealing the package HUnit, executing the test-function and also all eval-expresions run the stuff below. See also https://github.com/ndmitchell/ghcid 

```powershell
stack install ghcid
cd src
stack exec ghcid -- --command='stack ghci Day17.hs --package HUnit' --allow-eval --test test
```

## Bugs
If you by chance get strange output when running the code, consider `stack --color never <filename>`. See https://stackoverflow.com/questions/48597590/why-does-stack-give-wierd-charcter-encoding-in-error-output-on-windows/48597683#48597683. 



# Profiling Haskell

This is well developed, but good tutorials are hard to come by. I've seen this relevant reading:

- Video tutorial with GHC https://www.youtube.com/watch?v=pwcEUdf4Qmk
- Stack tool profiling options. https://docs.haskellstack.org/en/stable/GUIDE/#debugging 
- Pasing other options to GHC when calling Stack https://docs.haskellstack.org/en/stable/GUIDE/#ghc-options
- GHC opn profiling https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html


### Time profiling
```
stack build --executable-profiling
stack exec --profile -- aoc2016 <dayNr> +RTS -p -hc
cat aoc2016.EXE.prof
```

### Space profiling
Assumes that you dont have a ps-viewer, but you have installed the ps2pdf program instead. :)
```
stack build --executable-profiling
stack exec --profile -- aoc2016 <dayNr> +RTS -hc
stack exec hp2ps -- aoc2016.EXE
ps2pdf aoc2016.EXE.ps
open aoc2016.EXE.pdf
```

If you don't have ps2pdf, you can get SVG  output with `stack install hp2pretty` and then
```
stack build --executable-profiling;
stack exec --profile -- aoc2016 <dayNr> +RTS -hc; 
stack exec hp2pretty -- aoc2016.EXE.hp; open aoc2016.EXE.svg
```

This is a cost centre based report. change the `-hc` to `-hT` or `-hy` to get varous report types.

### Code coverage
The standard setup described in 
https://docs.haskellstack.org/en/stable/coverage/#code-coverage
is not suitable for this package, since the library is compiled into the executable.
Instead, we must use GHC coverage options directly.

```powershell
stack build --ghc-options -fhpc # both builds the main binary so that it emits coverage data when run
aoc2016 <dayNr1> # run whatever days you like
aoc2016 <dayNr2> # run whatever days you like
stack exec hpc -- markup aoc2016.EXE.tix # compile the report from the days you ran
open hpc_index.html # look at the report
```

To get coverage for the tests, I have only managed a strange workaround. The Test runner is a temporary script Main.hs. So we can get the report for the test script. But not its libraries. Very strange.

```powershell
stack test --ghc-options -fhpc
open Main.hs.html
```

### Documentation
Some Modules (e.g. Day 14) script have documentation in them. Compile them with Haddock.
Read on that page for all options. https://haskell-haddock.readthedocs.io/en/latest/index.html

```powershell
stack install haddock
stack exec haddock -- --html  src/Day14.hs
open Day14.html
```

I guess one can set it up smarter, with some dedicated output folder and so on... but wth.