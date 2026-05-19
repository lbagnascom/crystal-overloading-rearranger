# Crystal overloading rearranger

A tool for analyzing overload resolution behavior in [Crystal](https://crystal-lang.org/) by generating all permutations of overloaded method definitions and running them through the compiler.

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) (tested with GHC 9.10 / Cabal 3.0+)
- [Crystal](https://crystal-lang.org/install/) (latest)

> **Optional** A [devenv](https://devenv.sh/) / Nix-based setup is included (see `devenv.nix`) and will provide all dependencies automatically.

## Build

```sh
cabal build
```

## Usage

The tool exposes two subcommands.

### `parse`

Checks whether a `.cr` file is parseable by this tool and prints the parsed result:

```sh
cabal run crystal-parser -- parse <file.cr>
```

This is useful for verifying that your file falls within the [supported subset](#parser-limitations) of Crystal before running `rearrange`.

### `rearrange`

Generates all permutations of `@[Slot]`-annotated functions/methods in a file (or every `.cr` file in a directory), runs each permutation through the Crystal compiler, and saves the results.

**Single file:**
```sh
cabal run crystal-parser -- rearrange <file.cr>
```

**Directory:**
```sh
cabal run crystal-parser -- rearrange <directory/>
```

#### Marking overloads for rearrangement

Annotate the functions or methods you want to permute with `@[Slot]`:

```crystal
class Foo
  @[Slot]
  def bar(x : Int32)
    x * 2
  end

  @[Slot]
  def bar(x : String)
    x + x
  end
end
```

Only definitions annotated with `@[Slot]` are reordered, everything else stays in place.

## Output

For each input file `<name>.cr`, a directory `<name>/` is created containing:

- `1.cr`, `2.cr`, … — one `.cr` file per permutation of the slots.
- `result.md` — a summary report with exit codes, stdout, and stderr for each permutation, compiled once per crystal opts subset we are interested in.

## Samples

The `samples/` directory contains interesting Crystal cases used for analysis: programs where overload ordering has a noticeable effect on compiler behavior. To run all of them:

```sh
cabal run crystal-parser -- rearrange samples/
```

## Development

A `ghcid`-powered watch loop (reloads on file changes and runs the test suite) is available via:

```sh
make repl
```

## Parser Limitations

This tool only parses a small subset of Crystal,  enough for the analysis it performs. Supported constructs include:

- **Classes** with an optional superclass (`class Foo < Bar`)
- **Functions and methods** with typed/untyped arguments, default literal values, and `forall` type variables (with a very simple body)
- **Annotations** (e.g. `@[Slot]`)
- **Literals**: integers, strings (`"..."`), booleans (`true` / `false`)
- **Line comments** (`#`)

Function bodies are captured as raw text and are not parsed. Anything not recognized at the top level (modules, structs, macros, complex expressions, etc.) is passed through as-is and will not be rearranged.
`
