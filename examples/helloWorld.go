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
