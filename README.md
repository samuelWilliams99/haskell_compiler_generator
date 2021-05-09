# Haskell compiler generator
This project generates Haskell compilers from formal language definitions for syntax and semantics.

## Requirements
The syntax handling for this project uses the [haskell_parser_generator](https://github.com/samuelWilliams99/haskell_parser_generator).
You may either copy this projects source code onto your computer and use `ghc`s `-i` flag to specify its location, or install the package via `Hackage` (awaiting upload).
If you wish to use this project, it is highly recommended you first read the README for the parser generator.

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

If the generated compiler supports includes (as defined [here](#meta-definitions)), the `--dependency DIR` or `-d DIR` option can be used to add directories for the compiler to look for source files in.

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
The smt file is split into the following 4 sections:

### Meta-definitions
The meta definitions for semantics encompass not only the semantics of the language, but also information about the compiler main file, code generation, and even affect the gmr definition requirements.
There are 5 meta definitions, which are sensitive to order, but not all are required.
- `%extension`- This directive is required and specifies the file extension for inputs to the generated compiler. This is given as a lowercase identifier and is often 1-3 characters. For example: `%extension mt`.
- `%imports`- This directive is optional and takes a `CodeBlock` which is inserted directly after the imports section of the output file. Imports cannot be placed in the `%precode` directive below, as other code is run before it.
- `%precode`- This directive is optional and takes a `CodeBlock` which will be inserted into the output semantics checker file. This should be used for defining any custom getters you require and building the standard environment.
- `%outputprecode`- This directive is optional and expects expects a string expression in Haskell (as a `CodeBlock` or lowercase identifier), equal to any C code you wish to be included at the start of the generated compiler's output. This would include any C libraries required or functions needed by the environment. For Example:
    `%outputprecode { "#include <stdio.h>" }`
    This would ensure all outputted C files first import the `stdio.h` library.
- `%hasincludes`- This directive is a flag, taking no argument and altering the behaviour of the compiler simply by being present. Thus, vacuously, it is optional.

By including the `%hasincludes` flag, the gmr file must specify whatever includes the parsed code has, this is done by returning a tuple of the tree and a list of import structures at the root of the gmr file, rather than just the tree.
This is best visualised by adding an extra rule at the top of the gmr file in the following structure:
```
Root :: Include* Command { (v2, v1) }
```
`v1` here is expected to be a list of tuples, including the name/path to another file (the extension will be automatically added to the end of the path is not already), and an `IncludeMap` data type.\\
The `IncludeMap` type is used for specifying what variables and functions are passed to the new environment.
The options for `IncludeMap` are as follows:
- `everything :: IncludeMap` - This copies the entire top level environment from the included file to the base file.
- `whitelist :: [String] -> IncludeMap` - This takes a list of strings and only imports variables and functions with those names.
- `blacklist :: [String] -> IncludeMap` - Similar to whitelist but removes variables and functions instead.
- `rename :: [(String, String)] -> IncludeMap` - This renames any variables or functions named with the first `String` in the tuple with the second.

These options can be chained together using the `` `andThen` `` operator.

Following the `Root` example given above, a simple definition for `Include` could be:
```
Include :: include identifier   { (v2, everything) }
```

### Types
The following directives are used for specifying base and paramtrised types. These are ordered:
- `%basetype` - This directive is used to define a base type for the input language, and for specifying the equivalent C type. At least one of these definitions is required, for example: `%basetype boolean "int"`
- `%paramtype` - This directive is used to define parametrised types, which is a type with a name and a list of other types, for example, a simple array could have the name `"array"`, and the parameter list of a singleton.
The second argument is a function that maps a list of strings (the C types for the parametrised type) to a new C type. Formally, its type is `[String] -> String`.
There can be any number of `%paramtype` definitions, including zero.

### Environment
The environment features a variable `HashMap` with scope and name shadowing internally supported. It keeps track of its current scope automatically and also allows for functions to be defined in a way that swaps support for function pass-by-reference with function polymorphism, via a separate `HashMap` from name and argument types to list of variables.

The variable (`Var`) structure keeps track of most information needed in regular languages, such as the scope it was defined in, a generated C name for that var, and its type.

Both the environment (or state) and the variables allow for user defined "extra" information on the record. These are defined via the `%stateextra` and `%varextra` directives explained at the end of this section. They are automatically passed to where they could be needed, however the you will have to define your own getters for these, as those cannot be generated neatly.

There are 4 different forms of types, given below:
- `CommandType` - This meta-type is for any expression or command that does not return a type. It can be seen as C's "void" and is used as the default for commands. For example, a variable assignment, or for loop would have the type `CommandType`. It is very uncommon to have a variable saved as a `CommandType`.
- `BaseType String` - These are defined from the `%basetype` directive, they map to the C type defined with them when converted to C code.
- `ParamType String [VarType]` - This meta-type encompasses parametrised types. This is a recursive data type, as it is possible to have for example, an array of arrays.
This type is mapped to C code using the functions defined with them as discussed above.
- `FuncType [VarType] VarType` - This meta-type is intended for a language with pass-by-reference functions, taking a list of argument types and an output type. It is expected that the language will either use this meta-type or the staticFunctions `HashMap` on the state, but not both.

The generated semantics checker uses the \ic{StateResult} type, defined as:
```haskell
type StateResult a = StateT SemanticsState Result a
```

Where `SemanticsState` includes the `VolatileState` (which contains the variables and functions `HashMaps` and `%stateextra` definitions), as well as the `PersistentState`, used for tracking information like a unique name counter.
`Result` here is effectively a `Maybe` monad with an error string.

When building the default environment, you will define a function of type `StateResult a`, where `a`, the return type, is discarded. You will then use the helper functions defined [here](#environment-functions) and the function `modEnv`, to setup variables and functions. The `modEnv` function is used to directly modify the current volatile state, rather than the globally defined `env` variable they are intended for, when used for the reductions.

With the environment defined, we can specify the 3 directives, order sensitive:
- `%stateextra` - Specifies the data type of an extra field on the state. It is required that this data type is an instance of `Monoid`, for the initial state and for state merging with includes (regardless of whether the language actually supports includes).
This directive is optional and defaults to `()`.

- `%varextra` - Specifies the data type of an extra field on variables, specifically those used in the variables `HashMap` on the state. There are no instance requirements on this type, as you are required to supply it when you add variables to the environment.
Similar to `%stateextra`, this directive defaults to `()`.

- `%standardenv` - Takes a function of type `StateResult a`, as discussed above. The function can be defined as a lowercase identifier or a `CodeBlock`.
This directive is optional, and defaults to an empty environment.

### Reductions
The rest of the smt file is dedicated to defining the reductions. These are semantic reductions for structures outputted by the parser, and define code generation, output types, and any dependency before said reduction can pass.
Reductions are defined in groups, by the ddata type of the input. This is specified using the `%asttype` directive, which should be called before each group of reductions with the data type they are reducing.

The minimum requirements for a reduction are as follows:
- The input pattern, matching a constructor for the current `%asttype`.
It is required that the pattern defines `ps` as a `ParseState` (see the parser generator documentation for more information about this), this will normally be the `ParseState` in each of the AST constructors defined in the gmr file. For example: `{ ASTExprInt x ps }`.
- The output code, this will be the Haskell code to generate the relevant C code for this reduction, there are several presets for code generation to minimise the required knowledge of C.
- The output type, this will be the type of this expression or command. If the output type is not specified, it will default to a `CommandType`. If it is a string literal, it will be wrapped as a `BaseType` of that string. Otherwise, if a `CodeBlock` is used, you are expected to return a `VarType` object.

Using only the above features, axiom reductions can be defined, as such:
```
case { ASTExprInt x ps } -> { cInt x } @ "int"
```
Each reduction must start with the `case` keyword, followed by the input pattern, an arrow `->`, the output code (in this case we are using the `cInt` preset function, see [here](#c-code-generation-presets)), and lastly the type, preceeded by a `@`.
This example axiom says that literal integers in the input language are treated as the base type "int" in semantics.

If we wish to alter the output environment for this reduction, we must use the `~` character, followed by the name of a new `VolatileEnvironment`, as such:
```
case { ASTExprInt x ps } -> { cInt x } ~ env' @ "int"
```
Note that the environment should always come before the type.

To define non-axiom reductions, we need a way to reduce parts of the input token before finishing the current reduction. This is done using the `evaluating` section.
The `evaluation` section should contain a list of dependency reduction, defined in the following format:
```
inputEvaluable ~ inputEnv -> outputCode ~ newEnv @ "expectedType"
```
The `inputEvaluable` here is expected to either be one of the `%asttype` data structures, or a `Foldable` of one of these structures.  
The `inputEnv` and `newEnv` parameters are optional, simply omit the identifier and the `~` if you wish not to specify. Leaving the input environment blank will default it to the current environment, denoted by the pre-defined `env` variable. Leaving the output environment blank will simply discard this environment.  
The `->` defined the type of reduction to perform. For single evalable structures, such as in the example, the default `->` should be used. However, if `inputEvaluable` is a `Foldable` of evaluables, the `*->` or `^->` arrow should be used. The `*->` arrow will pass the same environment to the reduction of each of the contained evaluables, whereas the `^->` arrow will propagate the changes to the environment while folding over the structure.  
Lastly, `expectedType` here is formatted the same as with the output type for the containing reduction, where the expected type will be compared to the real type, and error if they are different. There is one addition however, if you use a normal identifier for the type, rather than a string literal or codeblock, the output type of the dependency will be assigned to that name for use later. Reuse of the same identifier for several dependencies will ensure equality of the output types for these dependencies.

Note here that both `inputEvaluable` and `inputEnv` can be `CodeBlocks`, allowing you to generate the value required with haskell code inline.

When using identifier types, you can use the `restricting` section to ensure enforce their value, as follows:
```
evaluating
    val1 -> val1S @ valType
    val2 -> val2S @ valType
restricting
    valType to int, boolean
```
This ensures both `val1` and `val2` reduce to the same type (as discussed above), but then also enforces this type to be either an `int` or `boolean`, which are both base types in this hypothetical language.

Finally, the last section we can define on a reduction is the `where` section, which takes a `CodeBlock` and runs within the reduction (a `StateResult` computation). This is used to manipulate the environment and enforce custom requirements on the input. The list of functions you can use in this section are listed in the section below.

### Environment functions
There are two types of environment functions, those that change the environment and those that simply read from it. To ease explanation of these functions, we will use the `VolatileStateMap` type, defined as follows:
```haskell
type VolatileStateMap = VolatileState -> VolatileState
```
Functions that manipulate the environment will either return `VolatileStateMap` or a `VolatileState` in some capacity.
Lastly, we have a set of functions for dealing with failure.

These functions are listed and explained below:

#### Environment Manipulators
- `increaseScope :: VolatileStateMap` - Increase an environments scope, new variables defined from here will be created in this scope.
- `decreaseScope :: VolatileStateMap` - Decreases the environment scope, any functions or variables defined in the old scope will be removed.
- `modifyVar :: Var VarExtra -> (Var VarExtra -> Var VarExtra) -> VolatileStateMap` - Maps a function onto a variable in the environment.
- `addStaticFunc :: String -> [VarType] -> VarType -> String -> VolatileStateMap` - Adds a static function to the environment.
  ```
  addStaticFunc name argTypes returnType cFunction env
  ```
- `addVarFunc :: String -> [VarType] -> VarType -> VarExtra -> String -> VolatileStateMap` - Adds a variable with the function type to the environment
  ```
  addVarFunc name argTypes returnType () cFunction env
  ```
- `addVar :: String -> VarExtra -> VarType -> VolatileState -> StateResult (Var VarExtra, VolatileState)` - Adds a generic variable to the environment, returns the variable object.
  ```
  addVar name () varType env
  ```

Normally in our reductions, we pass the environment around ourselves, however when building the standard environment, this gets rather tedious. Instead, the `modEnv` function is implemented, which simply takes a `VolatileStateMap` and applies it to the current State. This should only be used for creating the standard environment, and not in `where` clauses of reductions.
```
modEnv :: VolatileStateMap -> StateResult ()
```
#### Environment Accessors
- `getVar :: String -> VolatileState -> Maybe (Var VarExtra)` - Gets a variable by name, picks the variable with the highest scope, or gives nothing if no variable exists.
- `getStaticFunc :: String -> [VarType] -> VolatileState -> Maybe (Var ())` - Gets a static function by name and arguments.
- `getVarFunc :: String -> VolatileState -> Maybe (Var VarExtra, [VarType], VarType)` - Gets a variable function by name, also returns argument and return types.

#### Failure functions
- `err :: String -> StateResult a` - Simply errors, the error message passed in will have the source position added to it, along with the file path if `%hasincludes` is present.
- `require :: String -> Bool -> StateResult ()` - Takes an error and a boolean, errors if the boolean is false with the given error message.
- `forceMaybe :: String -> Maybe a -> StateResult a` - Takes a `Maybe` value, and returns its contents if it is `Just a`, or errors with given error message if it is `Nothing`.

### C Code Generation Presets
Following is a list of C-preset code generators, all returning strings.
- `indent :: String -> String` - Indents a section of code by four spaces on each line.
  ```
  > indent "hello\nworld"
      hello
      world
  ```
- `cBinOp :: String -> String -> String -> String` - Application of a binary operator to two arguments.
  ```
  > cBinOp "1" "2" "+"
  (1) + (2)
  ```
- `cUnOp :: String -> String -> String` - Application of a unary operator to one argument.
  ```
  > cUnOp "1" "-"
  (-(1))
  ```
- `cInt :: Int -> String` - Conversion of a Haskell `Int` to a C compliant `int`.
  ```
  > cInt 1
  1
  ```
- `cFloat :: Float -> String` - Conversion of a Haskell `Float` to a C compliant `float`.
  ```
  > cFloat 3.14
  3.14
  ```
- `cBool :: Bool -> String` - Conversion of a Haskell `Bool` to a C `int`, as `0` or `1`.
  ```
  > cBool True
  1
  ```
- `cVar :: Var a -> String` - Extracts the generated internal C name of a `Var`.
  ```
  > cVar myVar
  var0
  ```
- `cAssignVar :: Var a -> String -> String` - Assignment to an existing `Var`, this will not work if `cCreateVar` has not been called on this `Var` before.
  ```
  > cAssignVar myVar "1"
  var0 = 1;
  ```
- `cCall :: Var a -> [String] -> String` - Calls a function with a list of arguments, discarding the output.
  ```
  > cCall myFuncVar ["1"]
  var0(1);
  ```
- `cCallExpr :: Var a -> [String] -> String` - Calls a function inline, keeping the return value.
  ```
  > cCallExpr myFuncVar ["1"]
  var0(1)
  ```
- `cBlock :: String -> String` - Increase the C scope and indents.
  ```
  > cBlock "hello\nworld"
  {
      hello
      world
  }"
  ```
- `cSimpleIf :: String -> String -> String -> String` - Takes a condition, true command false command, builds a simple `if ... then ... else ...`.
  ```
  > cSimpleIf "1" "hello" "world"
  if(1){
      hello
  } else {
      world
  }
  ```
- `cIf :: [(String, String)] -> Maybe String -> String` - Allows for `if ... then ... elseif ... then ... else ...` chains, taking a list of condition and command pairs, then an optional else command.
  ```
  > cIf [("1", "hello"), ("0", "world")] Nothing
  if(1){
      hello
  } else if(0){
      world
  }
  ```
- `cSeq :: [String] -> String` - Chains together commands.
  ```
  > cSeq ["hello", "world"]
  hello
  world
  ```
- `cPass :: String` - Does nothing, useful for the `pass` keyword in some languages.
  ```
  > cPass
  ""
  ```
- `cCreateVar :: Var a -> Maybe String -> String` - Creates a variable in the C code, should be used in conjunction with `addVar` defined [here](#environment-manipulators). Automatically generates the C type code for the `Var` passed in.
  ```
  > cCreateVar myVar (Just "1")
  int var0 = 1;
  ```
- `cWhile :: String -> String -> String` - Takes a condition and command, builds a simple while loop.
  ```
  > cWhile "1" "hello"
  while(1){
      hello
  }
  ```
- `cSimpleFor :: Var a -> String -> String -> String -> String -> String` - Takes a `Var`, start value, end value and step, builds a simple counting for loop.
  ```
  > cSimpleFor myVar 0 5 1 "hello"
  int limit = 5;
  for(int var0 = 0; var0 < limit; var0 += 1){
      hello
  }
  ```
- `cFor :: Var a -> String -> String -> String -> String -> String` - Takes a `Var` and start value, then a condition and step command, for a for loop more similar to C's.
  ```
  > cFor myVar "0" "var0 < 5" "var0++" "hello"
  for(int var0 = 0; var0 < 5; var0++){
      hello
  }
  ```
