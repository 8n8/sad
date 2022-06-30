package main

import (
	"errors"
	"fmt"
	"io/fs"
	"net/http"
	"os"
	"sync"
	"time"
)

var GO = make(chan func(), 1)

func main() {
	GO <- ReadFile("db").RUN
	for {
		go (<-GO)()
	}
}

type ReadFile string

func (r ReadFile) RUN() {
	contents, err := os.ReadFile(string(r))
	UPDATEIO(fileContents{string(r), contents, err})
}

type fileContents struct {
	path     string
	contents []byte
	err      error
}

var dbPath string = "db"

func dumpCache(i uint16) Cmd {
	return WriteFile{
		path:     dbPath,
		contents: encodeUint16(i),
	}
}

func encodeUint16(i uint16) []byte {
	return []byte{
		byte(i & 0xFF),
		byte((i >> 8) & 0xFF),
	}
}

type WriteFile struct {
	path     string
	contents []byte
}

func (w WriteFile) RUN() {
	_ = os.WriteFile(w.path, w.contents, 0600)
}

func (f fileContents) update(model Model) (Model, Cmd) {
	var startServer startHttpServer = startHttpServer{
		Addr:           ":3000",
		ReadTimeout:    10 * time.Second,
		WriteTimeout:   10 * time.Second,
		MaxHeaderBytes: 1 << 20,
	}

	if errors.Is(f.err, fs.ErrNotExist) {
		var cmds Cmds = Cmds([]Cmd{
			dumpCache(0),
			startServer,
		})

		return Model(0), cmds
	}

	fromDisc, err := decodeUint16(f.contents)
	if err != nil {
		return model, Panic(err.Error())
	}

	return Model(fromDisc), startServer
}

func decodeUint16(raw []byte) (uint16, error) {
	if len(raw) != 2 {
		return 0, errors.New("raw cache not 2 bytes")
	}

	return uint16(raw[0]) + uint16(raw[1])*256, nil
}

type CmdNone struct{}

func (CmdNone) RUN() {
}

func (p Panic) RUN() {
	panic(p)
}

type Panic string

type startHttpServer struct {
	Addr           string
	ReadTimeout    time.Duration
	WriteTimeout   time.Duration
	MaxHeaderBytes int
}

type Model int

var MODEL Model = 0

var LOCK sync.Mutex

type Cmd interface {
	RUN()
}

type Msg interface {
	update(Model) (Model, Cmd)
}

func UPDATEIO(msg Msg) {
	var cmd Cmd
	LOCK.Lock()
	MODEL, cmd = msg.update(MODEL)
	LOCK.Unlock()
	cmd.RUN()
}

type HTTPHANDLER struct{}

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

type httpRequest struct {
	Method     string
	Path       string
	Header     map[string][]string
	RemoteAddr string
	Done       chan httpResponse
}

type httpResponse struct {
	Header     map[string][]string
	Body       []byte
	StatusCode int
}

func (h httpRequest) update(model Model) (Model, Cmd) {
	var newModel Model = Model(int(model) + 1)
	var message string = fmt.Sprintf("%d", int(model))
	var response httpResponse = httpResponse{
		Header:     make(map[string][]string),
		Body:       []byte(message),
		StatusCode: 200,
	}
	var cmds Cmds = Cmds([]Cmd{
		writeHttpResponse{response, h.Done},
		dumpCache(uint16(model)),
	})
	return newModel, cmds
}

type Cmds []Cmd

func (cmds Cmds) RUN() {
	for _, cmd := range cmds {
		cmd.RUN()
	}
}

type writeHttpResponse struct {
	Response httpResponse
	Done     chan httpResponse
}

func (w writeHttpResponse) RUN() {
	w.Done <- w.Response
}

type println string

func (p println) RUN() {
	fmt.Println(string(p))
}

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

type crashedHttpServer struct {
	err error
}

func (c crashedHttpServer) update(model Model) (Model, Cmd) {
	return model, println(fmt.Sprintf("%s", c.err))
}
