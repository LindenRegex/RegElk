# RegElk - OCaml Linear Engine for JavaScript Regexes
Authors: [Aurèle Barrière](https://aurele-barriere.github.io/) and [Clément Pit-Claudel](https://pit-claudel.fr/clement/).

## About
This is a linear regular expression engine for a subset of JavaScript regexes.
The underlying algorithm is an extension of the [PikeVM](https://swtch.com/~rsc/regexp/regexp2.html), supporting more JavaScript features.
This engine implements the algorithms described in the paper [Linear Matching of JavaScript Regular Expressions](https://arxiv.org/abs/2311.17620) by the same authors.

In particular, it supports, for the first time with linear time and space complexity:
- nullable JavaScript quantifiers (these have different semantics than in other regex languages, see for instance `(a?b??)*` on string "ab")
- capture reset, a JavaScript-specific property where capture groups are reset at each quantifier iteration (for instance `((a)|(b))*` on string "ab")
- all lookarounds (lookahads and lookbehinds), even with capture groups inside
- linear matching of the greedy or nullable plus.

RegElk means **Reg**ex **E**ngine with **L**inear loo**K**arounds. 
Elks are [diagonal walkers](https://ecowellness.com/animal-tracking-part-2-common-gait-patterns/), meaning that they reuse their front legs prints for their rear legs to conserve energy, evoking how a PikeVM merges threads reaching the same state to preserve linearity.

![RegElk](etc/regelk_logo.jpg)

## Complexity

Given a regex of size `|r|` and a string of size `|s|`, this engine has linear worst-case time complexity in both of them `O(|r|*|s|)`.
While counted quantifiers are supported, they increase the regex size.
For instance, `e{4-8}` will multiply the size of `e` 8 times.
However, the greedy plus (`+` or `{1,}`) or the nonnullable lazy plus (as in `(ab)+?`) are handled without duplication.

The engine also has `O(|r|*|s|)` space complexity.
If one wants to avoid a string-size dependent space complexity, we provide alternative register data-structures, presenting various time-space complexity tradeoff.

|                | Time Complexity             | Space Complexity |
|----------------|-----------------------------|------------------|
| List (default) | `O(\|r\|*\|s\|)`            | `O(\|r\|*\|s\|)` |
| Array          | `O(\|r\|^2*\|s\|)`          | `O(\|r\|^2)`     |
| Tree           | `O(\|r\|*log(\|r\|)*\|s\|)` | `O(\|r\|^2)`     |

Note however that a `O(|r|*|s|)` space complexity cannot be avoided when using our linear lookaround algorithm.

## Supported Features

| Feature                       | Example                                   |
|-------------------------------|-------------------------------------------|
| Lookaheads                    | `a(?=(b))`, `a(?!=b)`                     |
| Lookbehinds                   | `(?<=b)a`, `(?<!b)a`                      |
| Capture Groups                | `(a*)b`                                   |
| Noncapturing Groups           | `(?:a*)b`                                 |
| Greedy Quantifiers            | `*`, `+`, `?`                             |
| Lazy Quantifiers              | `*?`, `+?`, `??`                          |
| Counted Quantifiers           | `a{6,12}`, `a{7,}`, `a{9}`, `a{4,5}?`     |
| Character Classes             | `[a-z]`, `[^h]`, `[aeiouy]`               |
| Character Groups              | `\w`, `\d`, `\s`, `\W`, `\D`, `\S`        |
| Anchors                       | `$`, `^`                                  |
| Word Boundaries               | `\b`, `\B`                                |

Backreferences are not supported, as they make the matching problem [NP-hard](https://perl.plover.com/NPC/NPC-3SAT.html).
Named capture groups, hexadecimal escapes, unicode escapes, unicode properties and regex flags are not supported yet, although they could be in the future.


## Dependencies

Install all dependencies with Opam by running `opam install .`

You also need to install Node.JS and have `node` in your path.

## Usage
Build all executables with `make`. 
Make sure to have configured your opam switch so that it has all dependencies listed above.
This creates several executables:

- `main.native` is the Ocaml matcher
- `fuzzer.native` is a fuzzer that compares the OCaml matcher to the Irregexp engine of V8 in Node
- `tests.native` contains a battery of tests that should all succeed
- `stats.native` computes regex feature usage statistics from corpora of regexes
- `benchmark.native` allows you to run benchmarks
- `matcher.native` and `linearbaseline.native` are only used for the benchmarks and you should not run them directly


`main.native`, `fuzzer.native` and `benchmark.native` have command line options that can be printed with the argument `--help`.

## Files

### OCaml Engine Files
- the main entry point of the engine is in the `main.ml` file
- regexes and regex annotation are found in `regex.ml`
- the extended bytecode NFA representation is defined in `bytecode.ml`
- compilation from a regex to bytecode is found in `compiler.ml`
- the NFA simulation algorithm, with all our extensions, is implemented in `interpreter.ml`
- the CDN plus formulas are defined in `cdn.ml`
- the oracles used y the lookaround algorithm is defined in `oracle.ml`
- the three capture registers implementations are defined in `regs.ml`
- character classes are implemented in `charclasses.ml`
- the ECMA-style regex parser is defined in `parser_src/`

### Other Tools
- the differential fuzzer is implemented in `fuzzer.ml`
- the computation of statistics on regex features usage is defined in `stats.ml`
- the `scripts_bench` directory contains scripts called by the benchmarks or the fuzzer to compare the OCaml engine to other engines.

## Correspondence between the Paper and the Code

### Renamings
- The linear engine from V8 is called "V8Linear" in the paper. It is sometimes called "Experimental" or "Exp" in the code (as this is the name used by the V8 developers).
- The bytecode instructions `Consume` and `ConsumeAny` from Figure 4 are replaced by a single `Consume` instruction in `bytecode.ml`, which takes as argument either a character or a list of character ranges.
- The `Jump` instruction is called `Jmp` in the code.
- `SetReg` is called `SetRegisterToCP`.
- `SetQuant` is called `SetQuantToClock`.
- `CheckNull` is called `CheckNullable`.
- `SetNullPlus` in the paper is not an independent instruction in the code. Instead `SetQuantToClock` encodes both `SetQuant` and `SetNullPlus`: it takes a boolean argument indicating if this quantifier is a nulled plus or not.
- The "Balanced Tree" register implementation in the paper is renamed to `Map_Regs` in the code.

### Algorithm 1
This is implemented by the functions `advance_epsilon` and `find_match` in `interpreter.ml`.

### Section 4.1
- In `compiler.ml`, line 112, you can see the bytecode compilation of a quantifier and see that `BeginLoop` and `EndLoop` instructions are inserted.
- In `interpreter.ml`, the thread boolean `exit_allowed` encodes in which automata the thread is. See at lines 412 and 417 how the two new instructions are implemented.

### Section 4.2
- Threads are augmented with clocks in `quant_regs` (line 107 of `interpreter.ml`).
- The filtering algorithm is implemented at line 285 of `interpreter.ml`.

### Section 4.3
- The oracle table is defined in `oracle.ml`.
- The first phase is implemented as the `build_oracle` function, line 651 of `interpreter.ml`.
- The second phase is simply the `find_match` function defined previously.
- The third phase is implemented as the `build_capture` function, line 680 of `interpreter.ml`.

### Section 4.4
Switch to the `strlb` directory for this algorithm, and see the corresponding `README.md` file.

### Section 4.5
- The nullability analysis of Section 4.5.1 and Figure 12 starts at line 139 in `regex.ml`.
- The non-nullable plus case of Section 4.5.2 is implemented at line 78 in `compiler.ml`.
- For Section 4.5.3, see lines 90 and 101 of `compiler.ml`. 
- The nullability formulas of footnote 8 are defined in `cdn.ml` and called "CDN formulas".

### Section 4.6
- The three different register data-structures are defined in `regs.ml`: `Array_Regs`, `List_Regs` and `Map_Regs`.
- At line 28 of `interpreter.ml`, see that the interpreter is parameterized by a register implementation (a `REGS` module).
- The benchmark used for Figure 15 is defined as `dsarray`, `dslist` and `dstree` in `benchmark_vectors.ml`.

### Section 5.1
- All regex corpora are in the `corpus` directory.
- The `stats.ml` file uses the parser to analyze each regex and see in which categories it belongs to.

### Section 5.2
- (C1) the benchmark used for Figure 17 is named "Clocks", defined line 117 of `benchmark_vectors`.
- (C2) the benchmark used for Figure 18 is named "NNPlus", defined line 69.
- (C3) the benchmark used for Figure 19 is named "CDN", defined line 95.
- (C4) the benchmark used for Figure 20 is named "LBstr", defined line 179.
- (C5) the benchmark used for Figure 21 is named "LAreg", defined line 138.
- (C5) the benchmark used for Figure 22 is named "LAstr", defined line 157.

### Tests
- For every pair of regex and string discussed in the paper, we added this test to our test suite.
- This list is the `paper_tests` list at line 290 of `tests.ml`.
- This test suite is executed for each of the three register implementations when running `tests.native`.
