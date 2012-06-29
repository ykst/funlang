funlang
=======

A tiny language with following features:

 * lexical closure
 * gabage collection (simple mark and sweep)
 * custom infix operator
 * eval
 * exec (!!)

### Build

You need GHC to build.

```bash
$ make
```
Please fix other external dependencies manually if required.

### Syntax (EBNF + Regular expression)
```haskell
Program ::= {Statement "\n"}

Statement ::= 
 	  FunctionDef
	| InfixDef
 	| VariableDef
 	| IfBlock
 	| WhileBlock
 	| Return
 	| Assign
 	| CallAccess

Function ::= 
	"fun" "(" Varlist ")" "\n" 
 			Program
	End

End ::= /\\_+/

Varlist ::= epsilon | Id {"," Id}

Id ::= /[a-zA-Z][0-9a-zA-Z]*/

InfixDef ::= 
	InfixSpec "(" Id InfixOp Id ")" "\n" 
 		Program
	End

InfixSpec ::= 
	  /infixL[0-9]+/
	| /infixR[0-9]+/

InfixOp ::= /[!$%&<>?_*:;@/\\=|\-+.]+/

VariableDef ::= 
	"var" Id ["=" Expr]

IfBlock ::=
	"if" Expr "\n"
		Program
	{"elsif" Expr "\n"
		Program}
	["else" "\n"
		Program]
	End

WhileBlock ::= 
	"while" Expr "\n"
		Program
	End

Return ::= 
	"return" Expr

Assign ::= 
	Id "=" Expr

CallAccess ::=
	{Call "." | Id "."} Call

Call ::= 
	Method "(" Varlist ")"

Method ::= /\$?[a-zA-Z][0-9a-zA-Z]*/

Expr ::= 
	  Expr BinOp Expr 
	| UniOp Expr 
	| "(" Expr ")"
	| Terminal

Terminal ::= 
	  Call 
	| Instantiate
	| StringLiteral
	| IntLiteral
	| BoolLiteral

Instantiate ::= 
	Id "{" Varlist "}"

StringLiteral ::= ? C-like string literal ?

IntLiteral ::= /[1-9][0-9]*/

BoolLiteral ::= "true" | "false"
```

###Intrinsics

Every intrinsic functions are started by `$`. Our Helloworld is,

```
$print("Hello World\n")
```

Other supported intrinsics are:

* $eval(str) ... eval string.

* $gc() ... run gabage collection immediately.

* $exec(str) ... execute external command and get stdout string as return value.

* $error(str) ... throw an error. anyway, `catch` is not implemented :p

### Usage

The standalone binary (bin/fun) supports REPL by haskeline. Just run the binary without arguments.

```bash
$ bin/fun
> 1 + 1
2
> $print("Hello World!\n")
Hello World!
> :q
$
```
Following interpretor commands are supported:

* :q ... c  quit REPL.

* :l \<filename\> ... load a script file.

* :c ... initialize the runtime environment.

* :h ...  help.

To execute a script file, pass the filepath to the binary.

```
$ bin/fun hello.fun
```

### So what's all about?

Just for fun :)
