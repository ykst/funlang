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
Program ::= {Statement "Än"}

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
	"fun" "(" Varlist ")" "Än" 
 			Program
	End

End ::= /ÄÄ_+/

Varlist ::= É√ | Id {"," Id}

Id ::= /[a-zA-Z][0-9a-zA-Z]*/

InfixDef ::= 
	InfixSpec "(" Id InfixOp Id ")" "Än" 
 		Program
	End

InfixSpec ::= 
	  /infixL[0-9]+/
	| /infixR[0-9]+/

InfixOp ::= /[!$%&<>?_*:;@/ÄÄ=|Ä-+.]+/

VariableDef ::= 
	"var" Id ["=" Expr]

IfBlock ::=
	"if" Expr "Än"
		Program
	{"elsif" Expr "Än"
		Program}
	["else" "Än"
		Program]
	End

WhileBlock ::= 
	"while" Expr "Än"
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

Method ::= /Ä$?[a-zA-Z][0-9a-zA-Z]*/

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
$print("Hello WorldÄn")
```

Other supported intrinsics are:

* $eval(str) Åc eval string.

* $gc() Åc run gabage collection immediately.

* $exec(str) Åc execute external command and get stdout string as return value.

* $error(str) Åc throw an error. anyway, `catch` is not implemented :p

### Usage

The standalone binary (bin/fun) supports REPL by haskeline. Just run the binary without arguments.

```bash
$ bin/fun
> 1 + 1
2
> $print("Hello World!Än")
Hello World!
> :q
$
```
Following interpretor commands are supported:

* :q Åc  quit REPL.

* :l Ä<filenameÄ> Åc  load a script file.

* :c Åc initialize the runtime environment.

* :h Åc help.

To execute a script file, pass the filepath to the binary.

```
$ bin/fun hello.fun
```

### So what's all about?

Just for fun :)