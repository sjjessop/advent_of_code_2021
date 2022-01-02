# Advent of Code 2021


[![Test status](https://github.com/sjjessop/advent_of_code_2021/workflows/test/badge.svg)](https://github.com/sjjessop/advent_of_code_2021/actions)

Solutions to the problems from https://adventofcode.com/2021

Most of the code won't make much sense unless you've read the problem
description first.

I chose Scala to solve the problems this year. I've used fairly low-tech Scala
(no third-party libraries, for example), but I did occasionally use features
from Scala 2.13. I haven't packaged the code, so in the unlikely event you're
running this code, note that you have to compile day01 and day05 to run the
later days that depend on them. Otherwise each day runs independently.

You need to provide each daily input (from the site) as `day_XX_input.txt`, and
the correct answers (for checking) as `day_XX_answers.txt`. If you don't have
the answers already (because you haven't played yourself) then you could edit
`day01.scala` to remove the assertion that each answer is correct, or you could
build answer files one part at a time.

Advent of Code delivers the problems in two parts, with the second hidden until
the first is solved. In this repo each problem has (at least) two commits,
"part 1" and "part 2", to show how my solution developed from one to the other.

I have rebased some changes back into older commits - for example I was only
reminded near the end that Advent of Code prefers that players don't check
their input files and corresponsing answers into public repos, so I removed
those. Likewise I've rebased changes to add comments or make the code a bit
clearer. Changes made to support part 2 aren't rebased into part 1, though.
