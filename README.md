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

Most of them are `RUN` methods that execute IO. Some however are functions that are externally pure in that they don't do IO and don't alter their arguments, but do some mutation inside. These are used for looping and for operations on collections.

Run a bunch of commands:
```
func (cmds Cmds) RUN() {
	for _, cmd := range cmds {
		cmd.RUN()
	}
}
```

Read a line from STDIN:
```
func (fmtScanln) RUN() {
	var input string
	var n, err = fmt.Scanln(&input)
	UPDATEIO(fmtScanlnResult{input, n, err})
}
```

This is only used from other standard functions. It is a helper for updating the program state for a multi-threaded program.
```
func UPDATEIO(msg Msg) {
	var cmd Cmd
	LOCK.Lock()
	MODEL, cmd = msg.update(MODEL)
	LOCK.Unlock()
	cmd.RUN()
}
```

A helper for updating the state of single-threaded program. This is only called from other standard functions.
```
func UPDATEIO(msg Msg) {
	var cmd Cmd
	MODEL, cmd = msg.update(MODEL)
	cmd.RUN()
}
```

Print to STDIO, followed by a newline.
```
func (p fmtPrintln) RUN() {
	fmt.Println(string(p))
}
```

Declare the global lock. For protecting the state in a multi-threaded program.
```
var LOCK sync.Mutex
```

A handler for HTTP requests.
```
func (HTTPHANDLER) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	done := make(chan httpResponse, 1)
	request := httpRequest{
		Method:     r.Method,
		Path:       r.URL.Path,
		Header:     r.Header,
		RemoteAddr: r.RemoteAddr,
		Done:       done,
	}
	UPDATEIO(request)
	response := <-done
	for key, values := range response.Header {
		for _, value := range values {
			w.Header().Set(key, value)
		}
	}
	w.WriteHeader(response.StatusCode)
	_, _ = w.Write(response.Body)
}
```

Send a response to an HTTP request.
```
func (w writeHttpResponse) RUN() {
	GO <- func() {
		w.w.Write(w.message)
		w.done <- struct{}{}
	}
}
```

This is used by standard functions to send jobs to be executed in a separate goroutine.
```
var GO = make(chan func(), 1)
```

A `Msg` is an input from the outside world, i.e. the result of doing some IO. The `update` function is a pure function that updates the program state and decides what IO to do, based on an input.
```
type Msg interface {
	update(Model) (Model, Cmd)
}
```

A `Cmd` is an instruction to do some IO, created by a pure function. The `RUN` function executes the instruction.
```
type Cmd interface {
	RUN()
}
```

Puts an HTTP response into a channel so it can be sent by the receiver.
```
func (w writeHttpResponse) RUN() {
	w.Done <- w.Response
}
```

Reads a whole file into a byte slice.
```
func (r ReadFile) RUN() {
	contents, err := os.ReadFile(string(r))
	UPDATEIO(fileContents{string(r), contents, err})
}
```

Writes a whole file.
```
func (w WriteFile) RUN() {
	err := os.WriteFile(w.path, w.contents, 0600)
	UPDATEIO(fileWritten{err, w.path})
}
```

Doesn't do anything. Sometimes an `update` function only needs to update the program state and doesn't want to run any IO, in which case it can return `CmdNone` as its `Cmd`.
```
func (CmdNone) RUN() {
}
```

Crash.
```
func (p Panic) RUN() {
	panic(p)
}
```


Start up an HTTP server.
```
func (s startHttpServer) RUN() {
	sh := &http.Server{
		Addr:           s.Addr,
		Handler:        HTTPHANDLER{},
		ReadTimeout:    s.ReadTimeout,
		WriteTimeout:   s.WriteTimeout,
		MaxHeaderBytes: s.MaxHeaderBytes,
	}
	GO <- func() {
		UPDATEIO(crashedHttpServer{sh.ListenAndServe()})
	}
}
```

A general looping function. It keeps on updating `mutable` using `update` while `keepGoing` wants it to. Note that although this function has mutation interlly it is pure from a caller's perspective, and can therefore be used in pure functions.
```
func LOOP[T any](mutable T, update func(T) T, keepGoing func(T) bool) T {
	for keepGoing(mutable) {
		mutable = update(mutable)
	}
	return mutable
}
```

For inserting elements into a map. Or rather, it returns a copy of the original map with the new element in it. Like `LOOP`, this function is pure externally, so can be called from a pure function, even though it contains mutation.
```
func INSERT[K comparable, V any](key K, value V, oldMap map[K]V) map[K]V {
	var newMap = make(map[K]V)
	for k, v := range oldMap {
		newMap[k] = v
	}
	newMap[key] = value
	return newMap
}
```

For removing elements from a map. Or rather, it returns a copy of the original map but with the element removed. Again, this function is pure externally so can be called from a pure function, even though it contains mutation.
```
func DELETE[K comparable, V any](key K, oldMap map[K]V) map[K]V {
	var newMap = make(map[K]V)
	for k, v := range oldMap {
		newMap[k] = v
	}
	delete(newMap, key)
	return newMap
}
```
