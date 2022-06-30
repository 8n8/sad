package main

// hello
// how are you?

import (
	"fmt"
)

func main() {
	Print("hi").RUN()
}

type Print string // something goes here

func (p Print) RUN() {
	fmt.Println(string(p))
}
