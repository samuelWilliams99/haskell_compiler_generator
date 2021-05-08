# Haskell compiler generator
This project generates Haskell compilers from formal language definitions for syntax and semantics.

## Requirements
The syntax handling for this project uses the [haskell_parser_generator](https://github.com/samuelWilliams99/haskell_parser_generator).
You may either copy this projects source code onto your computer and use `ghc`s `-i` flag to specify its location, or install the package via `Hackage` (awaiting upload)

Documentation for this library has been generated using [haddock](https://www.haskell.org/haddock/). If you are modifying the documentation, and are using the parser generator library source files, please ensure that haddock does not include these files.

## Usage
The inputs to this project are in the form of a `.gmr` file (formally defined in the readme for the parser generator) and a `.smt` file (formally defined [here](#smt-format)
### Compiled
The compiled script can be run taking one argument, the path to both `gmr` and `smt` file, in the form of an extensionless path. The script will then generate the following four files, given the the input file name `NAME`:
- `NAMEcompiler.hs`
- `NAMEsemantics.hs`
- `NAMEparser.hs`
- `parserrequirements.hs`

For example, running:
`./compilergenerator "examples/mt"`
yields `mtcompiler.hs`, `mtsemantics.hs`, `mtparser.hs` and `parserrequirements.hs` all in the `examples` directory.

### Interpreted
The following functions have been defined in the `CompilerGenerator` module.
- `runCompilerGenerator :: String -> IO ()` - Taking the file path and creating the `hs` files where needed.
- `generateCompiler :: String -> String -> String -> Result (String, String, String)` - Taking the `gmr` contents, `smt` contents, and module name, and returning a tuple of parser code, semantics code and compiler code.

Further documentation on these files can be found in the haddock documentation.
### Generated compiler
The generated compiler can be run either interpreted or compiled. If compiled the following command structure should be used:
```
compiler [OPTION...] FILE
```

The following OPTIONS or flags are then supported, with shorthand versions taking the first character:
- `--extension` or `-e` - Gets the file extension the compiler accepts. This flag does not require the input file to be specified.
- `--info` or `-i` - Gives information about the compiler, mostly about its origin. This flag does not require the input file to be specified.
- `--verbose` or `-v` - Prints information about the compilation process.
- `--output FILE` or `-o FILE` - Allows the user to specify the output location.

If the generated compiler supports includes (as defined [here](#)), the `--dependency DIR` or `-d DIR` option can be used to add directories for the compiler to look for source files in.

If you choose to run the compiler interpreted, the following function is defined:
```haskell
runCompiler :: String -> Maybe String -> Bool -> IO ()
```
This takes the path to the input file, a maybe value for the output file, and a verbose boolean flag.
Similar to the compiled command, if the language supports includes, this function changes to:
```
runCompiler :: String -> Maybe String -> Bool -> [String] -> IO ()
```
The extra list of Strings here is the list of additional directories to check for source files.

Lastly, if you wish to compile without writing to a file, the following function can be used:
```haskell
compile :: Bool -> String -> IO String
```
This takes the verbose flag and the file path, and returns the compiled file as a String.
If using a includes-enabled compiler, this changes to:
```haskell
compile :: Bool -> [String] -> String -> IO String
```
Where, again, the extra list of Strings are the dependency directories.

## smt format
