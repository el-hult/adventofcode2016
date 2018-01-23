For making the AoC2016 advent calendar. http://adventofcode.com/2016

Please go there and take a look! :)


# Instructions

The code is mainly in haskell. I would recommend using it like below.

1. Download stack from https://haskellstack.org/
1. To run the program corresponding to a specific day, first `cd Day<nr>` and then `stack Day<nr><A|B>.hs`.

Be prepared that the first time you run a file, a lot of libraries will be downloaded.

In many folders, the files are named something else, but the above is what I hope to be the convention. :)

What happens is that `stack` reads the stack config in the beginning of each source file, provides the packages needed and runs the file in interpreted mode.

If the script is too slow (as of writing this, Day5 is very slow), one might want to compile and optimize the program. Run `stack script <filename> --optimize --resolver lts-<xx>` with `<filename>` being whatever script you want to run, and `<xx>` the lts version number that you find in the script header.

