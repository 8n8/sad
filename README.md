# Sad

**This is a work in progress, and doesn't work very well yet. It's not ready to use.**

Sad is a method of writing Golang in a pure functional style and a source code analyser to enforce it.

## Hello world

This is how to write a hello world program in Sad:

```
package main

import (
	"fmt"
)

func main() {
	fmtPrintln("hello world!").RUN()
}

type fmtPrintln string

func (p fmtPrintln) RUN() {
	fmt.Println(string(p))
}
```

## Installation

You have to install from source. This has been tested in Ubuntu Linux.

First install Git and the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Then:

```
git clone git@github.com:8n8/sad.git
cd sad
stack install
```

This can take a while, as it will download a compiler and the dependencies before building. The binary will be put in `~/.local/bin`, so you might want to add that path to your PATH in your shell configuration file. This file depends on which shell you use, but will be called `~/.bashrc` for Bash and `~/.zshrc` for Zsh. Add a line like this somewhere in this file:

```
export PATH=$HOME/.local/bin:$PATH
```

and then quit and reopen your shell or run `source /path/to/config/file`.

## Usage

Sad reads a Go source file from STDIN and prints to STDOUT. So to check a Go source file complies with Sad, do:

```
cat /path/to/file.go | sad
```

## Overview

Functions are divided into two kinds: pure and standard. Pure functions:

- cannot mutate their arguments
- cannot do IO
- cannot mutate variables, either global ones or those declared internally

So pure functions can only calculate a return value from their arguments, nothing else.

Standard functions are pre-defined, and must be reproduced character for character identically to the definition. The only exception is the main function, which has a little bit more flexibility.

These rules forbid most of the Go package ecosystem. The only imports allowed are a few functions from the standard library that are known to be pure or that are used in a standard function.

Sad expects that programs have been formatted with `gofmt`. It will fail for example if there are spaces instead of tabs in the indentation.

## Pure functions

An example of a pure function is:

```
func f(x int) int {
	var y = x * 2
	return y + 1
}
```

Note that it doesn't do any IO and doesn't mutate any variables.

A non-pure function like this wouldn't be allowed in Sad:

```
func f(x int) int {
	var y = x * 2
	fmt.Println("hello") // IO
	y = y + 3  // mutation
	return y
}
```

IO and mutation are only allowed in special predefined functions, described in the next section.

## Standard functions

These functions allow limited access to IO and mutation. It's necessary to have them because pure functions on their own are useless. Programs must do IO to be useful.

### The main function

Two kinds of main function are allowed, one for single-threaded programs, and one for multi-threading.

Here is an example of the single-threaded kind:

```
func main() {
	fmtPrintln("Hello world!").RUN()
}
```

So this is wrapping a string in a type called `fmtPrintln` (not to be confused with `fmt.Println` from the standard library) and calling a method called `RUN` on it.

This kind of main function must always have a body made of a pure value with a call to `RUN` attached to it.

The other kind of main function, for multi-threaded programs, is like this:

```
func main() {
	GO <- ReadFile("db").RUN
	for {
		go (<-GO)()
	}
}
```

So the only part of this that the programmer can change is the `ReadFile("db")` part. This can be any pure value. In this case, `ReadFile` is a type wrapper, defined like:

```
type ReadFile string
```

and `GO` is a global channel that is used to send functions to be run in a separate go-routine, defined like:

```
var GO = make(chan func(), 1)
```

Sad currently only uses goroutines to avoid being blocked by slow IO, so it might be suited for writing an HTTP server, with network and database accesses in separate threads.

### Predefined

These functions / types etc are predefined exactly, and to be accepted by Sad you must include them character-by-character as shown here if you want to use them.

```
func (cmds Cmds) RUN() {
	for _, cmd := range cmds {
		cmd.RUN()\n\
	}
}
```

```
func (fmtScanln) RUN() {
	var input string\n\
	var n, err = fmt.Scanln(&input)\n\
	UPDATEIO(fmtScanlnResult{input, n, err})\n\
}
```

**TODO** add the rest of these functions.
