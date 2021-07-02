For solving the AoC2016 advent calendar. http://adventofcode.com/2016

# Instructions

I stole much of my directory structure from (Tomash Aschan)[https://github.com/tomasaschan/advent-of-code-2018].
The migration to this structure is __in progress_ so only day 1 is in this structure, and the others are standalones that should be run interactively in their respective folders.

To just run a single day, install libraries with `stack setup`, compile all code with `stack build` and then run `stack exec aoc2016 -- X` for day `X`.

I ran `stack build --test --fast --file-watch --exec "aoc2016 X` where `X` is the day, to run test suites and my current day.

## Bugs
If you by chance get strange output when running the code, consider `stack --color never <filename>`. See https://stackoverflow.com/questions/48597590/why-does-stack-give-wierd-charcter-encoding-in-error-output-on-windows/48597683#48597683. 