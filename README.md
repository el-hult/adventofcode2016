For solving the AoC2016 advent calendar. http://adventofcode.com/2016

# Instructions

## Code structure and running solvers

The current folder structure is a mix between two:

1) A single package called `old` compiling a single executable that can solve many days depending on command line arguments. Run this with `stack build --test --fast --file-watch --exec "old <dayNr>"`.
   Because there are many days, and at each recompilataion, all previous days are linked into the same executable, this takes silly long time to compile. Also, all the tests are located in a single test executable, so a lot of unnecessary tests are rerun at each recompilation as well. Therefore, some code is migrated to a separate structure.
2) One package per day called `dayXX` where XX is `19`,`20`,`21` etc. Each package has their own executables, tests etc. To run these, do `stack build dayXX util --test --fast --file-watch --exec "dayXX"`.

Except from these packages, there is also a package `util` exposing the module `Util`. Since many days depend on this one, it can be very expensive to run `stack build --file-watch` and edit `util`. All days that use `util` well get recompiled and linked.

If you do *not* want file watch and test cases and so on, because you simply want to run the programs without developing, you can of course just `stack build` and then `stack exec dayXX` or `stack exec old -- XX`.

Many of these ideas were taken from  (Tomash Aschan)[https://github.com/tomasaschan/advent-of-code-2018], but I started out in other ways, hoping to find a simpler solution with fewer moving parts.
But it seems this is the best setup.

## Alternative running
Another way to run the code is with `ghcid`. I find it REALLY nice for these small things. 
To run Day17 upon saves, and revealing the package HUnit, executing the test-function and also all eval-expresions run the stuff below. See also https://github.com/ndmitchell/ghcid 

```powershell
stack install ghcid
cd old/src
stack exec ghcid -- --command='stack ghci Day17.hs --package HUnit' --allow-eval --test test
```

## Bugs
If you by chance get strange output when running the code, consider `stack --color never <filename>`. See https://stackoverflow.com/questions/48597590/why-does-stack-give-wierd-charcter-encoding-in-error-output-on-windows/48597683#48597683. 

If the code crashes with an error like `<stderr>: commitAndReleaseBuffer: invalid argument (invalid character)` it is because the shell wont accept utf8 characters https://stackoverflow.com/questions/63746826/what-might-cause-commitandreleasebuffer-invalid-argument-invalid-character .
I could resolve this with 
```powershell
[console]::InputEncoding = [console]::OutputEncoding = New-Object System.Text.UTF8Encodi
```
as sinspired from https://github.com/PowerShell/PowerShell/issues/7233 and I guess the solution depends on the PS version. I have 5.1 currently.

# Developing aid

Both `stylish-haskell` and ormolu are good variants for formatting. Ormolu is more opinionated, and what I have used. They both work with HLS for VS Code, but either one is really ok.

```powershell
stack exec hlint -- .
stack exec stylish-haskell -- . --recursive --inplace
stack exec ormolu -- some/file/path.hs --mode inplace
```

# Profiling Haskell

This is well developed, but good tutorials are hard to come by. I've seen this relevant reading:

- Video tutorial with GHC https://www.youtube.com/watch?v=pwcEUdf4Qmk
- Stack tool profiling options. https://docs.haskellstack.org/en/stable/GUIDE/#debugging 
- Pasing other options to GHC when calling Stack https://docs.haskellstack.org/en/stable/GUIDE/#ghc-options
- GHC opn profiling https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html
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
stack exec haddock -- --html  old/src/Day14.hs
open Day14.html
```

I guess one can set it up smarter, with some dedicated output folder and so on... but wth.

### Benchmarks
Benchmarks are made by writing custom programs, and denote them as "bench" build targets. See Day22 for an example.
The program itself is responsible for reporting relevant output via stdOut or writing reports to file.
One framework that helps in that is Criterion. http://www.serpentine.com/criterion/tutorial.html


### IDE support
I have been using VSCode with the haskell language server. It is generally good, can find, parse and present haddock snippets, autoformat with `ormolu` or `stylish-haskell` and more.
There is one problem though. By default, the build system (`stack` in my case) is supposed to explain to [hie-bios](https://github.com/haskell/hie-bios`) how various parts of the project fits together.
When there are several `Main` modules in the stack project, this mechanism fails.
This can be fixed, by manually configuring a [multi-cradle](https://github.com/haskell/hie-bios#multi-cradle) project so that each project package is handeled with its own `Main`-module.
You need to write the name of a build target there, so call `stack ide build-target` to list them. That is a way to debug the `package.yaml` files as well. :)

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