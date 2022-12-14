I am doing [Advent of Code 2022](http://adventofcode.com/2022).
(I also did [2021](https://github.com/Plutor/advent-of-code-2021))

I am also learning [Haskell](https://www.haskell.org/).

And I am again coding on Windows with [Sublime Text 4](https://www.sublimetext.com/) and [GitSavvy](https://github.com/timbrel/GitSavvy).

# Learnings

* Oh boy Haskell is going to be a learning curve. Day 1 probably took me an hour to wrap my head around.
* Haskell tried to get a little too cute with operators. The fact that you've got things like `.` and `$` and `\` are understandable, but `!!` to index a list? I have some dignity.
* Things I need to learn instead of just trying permutations until they work:
  1. What's IO? Why do IO functions need to be use with <- instead of let? Is this related to do/return?
  1. `.` and `$` seem to be useful in getting rid of `()`s but not always. Are there better ways to do paren-heavy nested calls than the way I'm doing them?
  1. Why are tuples so useless? (Especially with more than 2 elements?)
  1. Should I be creating objects to make code like day 5 a little more scrutable?
* Objects have annoying boilerplate no matter how I do them (with or without named properties). I prefer neither.
* The `... where ...` syntax is pretty useful, I should use that more.

## Days of note

* Day 8 was the first really challenging day. My first solution for part 1 was O(n^2) (or maybe worse?) and then part 2 made that infeasible. As of the 9th I still need to go back and redo part 2.
* I struggled with day 12 for a while. Functional programming really forces you to think about problems like this in terms of _transformations_. I had to wrap my head around what would turn a "step" in the solution into the next "step", and then kinda work from there.
* Around a middle of the month, I acknowledged I was not having as much fun as I wanted, so I've started doing only the days that I feel like doing. It's a tiny failure, but this isn't a job, it's for fun.