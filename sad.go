package main

import (
    "os"
)

func main() {
    ReadFileHandle{handle: os.Stdin, context: "code"}.RUN()
}

type ReadFileHandle struct {
    handle *os.File
    context string
}

func (r ReadFileHandle) RUN() {
    contents, err := io.ReadAll(r.handle)
    UPDATEIO(FileHandleContents{r.context, contents, err})
}

type FileHandleContents struct {
    context string
    contents []byte
    err error
}

func UPDATEIO(msg Msg) {
    var cmd Cmd
    MODEL, cmd := msg.update(MODEL)
    cmd.RUN()
}

type Model struct{}

var MODEL = struct{}{}

type Msg interface {
    update(Model) (Model, Cmd)
}

type Cmd interface {
    RUN()
}

func (f FileHandleContents) update() (Model, Cmd) {
    if f.err != nil {
        return struct{}{}, println(f.err.Error())
    }

    var i, err = parse(f.contents, 0)
    if err == nil {
        return struct{}{}, print(f.err.Error())
    }

    if i != len(f.contents) {
        var print = println(
            prettyError(i, f.contents, "expecting end of input"))
        return struct{}{}, print
    }

    return struct{}{}, nothing{}
}

func prettyError(i int, contents []byte, msg string) string {
    var prettyPos = getPrettyPos(i, contents)
    return Sprintf("
}

type PrettyPos struct {
    line int
    col int
    content []byte

}

func loop(state PrettyPos, step func(PrettyPos) PrettyPos, stop func(PrettyPos) bool) PrettyPos {
    for stop(state) {
        state = step(state)
    }
    return state
}
    

type WriteHandle struct{
    msg []byte
    handle *os.File
    context string
}

func (w WriteHandle) RUN() {
    n, err := w.handle.Write(w.msg)
    UPDATEIO(HandleWritten{n, err, w.context})
}

type HandleWritten struct {
    n int
    err error
    context string
}

func (HandleWritten) update(_ Model) (Model, Cmd) {
    return struct{}{}, nothing{}
}

type nothing struct{}

func (nothing) RUN() {
}

func println(msg string) Cmd {
    return WriteHandle{[]byte(msg + "\n"), os.Stdout, ""}
}
