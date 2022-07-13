package main

import (
	"fmt"
)

var GO = make(chan func(), 1)

func main() {
	Cmds([]Cmd{
		fmtPrintln("What is your first name?"),
		fmtScanln{}}).RUN()
}

type Cmds []Cmd

func (cmds Cmds) RUN() {
	for _, cmd := range cmds {
		cmd.RUN()
	}
}

type fmtScanln struct{}

func (fmtScanln) RUN() {
	var input string
	var n, err = fmt.Scanln(&input)
	UPDATEIO(fmtScanlnResult{input, n, err})
}

var MODEL Model = struct{}{}

type Model struct{}

type Cmd interface {
	RUN()
}

type Msg interface {
	update(Model) (Model, Cmd)
}

func UPDATEIO(msg Msg) {
	var cmd Cmd
	MODEL, cmd = msg.update(MODEL)
	cmd.RUN()
}

func (scanned fmtScanlnResult) update(model Model) (Model, Cmd) {
	if scanned.err != nil {
		return model, fmtPrintln("couldn't read the input")
	}

	return model, fmtPrintln("Hello " + scanned.input)
}

type fmtPrintln string

func (p fmtPrintln) RUN() {
	fmt.Println(string(p))
}

type fmtScanlnResult struct {
	input string
	n     int
	err   error
}
