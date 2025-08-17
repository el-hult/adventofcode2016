WORK IN PROGESS: 
- Day 01-18 done
- Day 19-21 needs refactoring / verification.
- Day 22 is started, but not finished.
- Day 23-25 are not started yet.

For solving the AoC2016 advent calendar. http://adventofcode.com/2016

Project structure inspired by (Tomash Aschan)[https://github.com/tomasaschan/advent-of-code-2018].

# Instructions

In 2025, I installed Stack using GHCUp https://www.haskell.org/ghcup/, so that HLS support worked nicer in my IDE (through GHCUp).
I also udated the stackage resolver so it uses a newer version of GHC that I actually could download and install on MacOS.

## Code structure and running solvers

One package per day called `dayXX`.
Each package has their own executables, tests etc. Run with `stack run dayXX` or `stack test dayXX`.

In development, you might want `stack build dayXX util --test --fast --file-watch --exec "dayXX"` instead.

There is also a package `util` exposing the module `Util`. 

# Developing aid

## Formatters

Use `ormolu`. It is opinionated. Command line call it using below.
There is a VSCode extension for it, but I did not get it to work with the multi-cradle and stack setup, so I just used the CLI directly.
```shell
stack exec ormolu -- --mode inplace  some/file/path.hs 
```


## Profiling Haskell

This is well developed, but good tutorials are hard to come by. I've seen this relevant reading:

- Video tutorial with GHC https://www.youtube.com/watch?v=pwcEUdf4Qmk
- Stack tool profiling options. https://docs.haskellstack.org/en/stable/GUIDE/#debugging 
- Pasing other options to GHC when calling Stack https://docs.haskellstack.org/en/stable/GUIDE/#ghc-options
- GHC own profiling https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html
- Profiling via Criterion and Stack. There is No full tutorial, bu combine https://stackoverflow.com/questions/37485522/how-to-use-criterion-with-stack with http://www.serpentine.com/criterion/tutorial.html to get somewhere.

### Time profiling
```powershell
stack build --executable-profiling
stack exec --profile -- aoc2016 <dayNr> +RTS -p -hc
cat aoc2016.EXE.prof
```

### Space profiling
Assumes that you dont have a ps-viewer, but you have installed the ps2pdf program instead. :)
```powershell
stack build --executable-profiling
stack exec --profile -- aoc2016 <dayNr> +RTS -hc
stack exec hp2ps -- aoc2016.EXE
ps2pdf aoc2016.EXE.ps
open aoc2016.EXE.pdf
```

If you don't have ps2pdf, you can get SVG  output with `stack install hp2pretty` and then
```powershell
stack build --executable-profiling;
stack exec --profile -- aoc2016 <dayNr> +RTS -hc; 
stack exec hp2pretty -- aoc2016.EXE.hp; open aoc2016.EXE.svg
```

This is a cost centre based report. change the `-hc` to `-hT` or `-hy` to get varous report types.

## Code coverage
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

## Documentation
Some Modules (e.g. Day 14) script have documentation in them. Compile them with Haddock.
Read on that page for all options. https://haskell-haddock.readthedocs.io/en/latest/index.html

```powershell
stack install haddock
stack exec haddock -- --html  old/src/Day14.hs
open Day14.html
```

## Benchmarks
Benchmarks are made by writing custom programs, and denote them as "bench" build targets. See Day22 for an example.
The program itself is responsible for reporting relevant output via stdOut or writing reports to file.
One framework that helps in that is Criterion. http://www.serpentine.com/criterion/tutorial.html

## Debugging

the GHCi debugger is quite nice. Run `stack ghci`, then load modules of choice, set breakpoints with `:break`, show available variables with `:show bindings` etc. 

See [The documentation on GHCi debugging](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-debugger)

Trace debugging can be helpful, see `Debug.Trace` at https://hackage.haskell.org/package/base-4.16.0.0/docs/Debug-Trace.html#v:trace

Example:

```haskell
import Debug.Trace
fib :: Integer -> Integer
fib 0 = trace "0 => 0" $ 0
fib 1 = trace "1 => 1" $ 1
fib n =
   let input = n
       output = fib (n-1) + fib (n-2)
   in trace (show input ++ " => " ++ show output) $ output
```


## IDE support
I have been using VSCode with the haskell language server. It is generally good, can find, parse and present haddock snippets, autoformat with `ormolu` or `stylish-haskell` and more.
There is one problem though. By default, the build system (`stack` in my case) is supposed to explain to [hie-bios](https://github.com/haskell/hie-bios`) how various parts of the project fits together.
When there are several `Main` modules in the stack project, this mechanism fails.
This can be fixed, by manually configuring a [multi-cradle](https://github.com/haskell/hie-bios#multi-cradle) project so that each project package is handeled with its own `Main`-module.
You need to write the name of a build target there, so call `stack ide build-target` to list them. That is a way to debug the `package.yaml` files as well.

Therefore, the `hie.yaml` must be carried around all the time. It is a bummer, but it is acceptable, I guess.

If you get the error 
```
Multi Cradle: No prefixes matched
pwd: C:\Users\Ludvig\Google Drive\Hobbyprojekt 2021\adventofcode2016
filepath: C:\Users\Ludvig\Google Drive\Hobbyprojekt 2021\adventofcode2016\day18\app\Main.hs
prefixes:
("./old",Stack {component = Just "old:exe:old", stackYaml = Nothing})
("./day19",Stack {component = Just "day19:exe:day19", stackYaml = Nothing})
("./day20",Stack {component = Just "day20:exe:day20", stackYaml = Nothing})
("./day21",Stack {component = Just "day21:exe:day21", stackYaml = Nothing})
```
or similar, it means that the file you have opened does not match any of the file path prefixes (e.g. folders) in the multi-cradle setup. Maybe you added a new package, without adding a hie cradle configuration?