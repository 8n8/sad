package main

import (
	"fmt"
	"net/http"
	"sync"
	"time"
)

var GO = make(chan func(), 1)

func main() {
	GO <- startHttpServer{
		Addr:           ":3000",
		ReadTimeout:    10 * time.Second,
		WriteTimeout:   10 * time.Second,
		MaxHeaderBytes: 1 << 20,
	}.RUN
	for {
		go (<-GO)()
	}
}

type startHttpServer struct {
	Addr           string
	ReadTimeout    time.Duration
	WriteTimeout   time.Duration
	MaxHeaderBytes int
}

type Model struct{}

var MODEL Model = struct{}{}

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

func (h httpRequest) update(model Model) (Model, Cmd) {
	var response httpResponse = httpResponse{
		Header:     make(map[string][]string),
		Body:       []byte("hello"),
		StatusCode: 200,
	}
	return model, writeHttpResponse{response, h.Done}
}

type writeHttpResponse struct {
	Response httpResponse
	Done     chan httpResponse
}

func (w writeHttpResponse) RUN() {
	w.Done <- w.Response
}

type httpResponse struct {
	Header     map[string][]string
	Body       []byte
	StatusCode int
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
