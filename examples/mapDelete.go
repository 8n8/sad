package main

import (
	"fmt"
)

func main() {
	fmtPrintln(getString()).RUN()
}

type fmtPrintln string

func (p fmtPrintln) RUN() {
	fmt.Println(string(p))
}

func getString() string {
	var init = map[int]string{0: "hi", 1: "hey", 2: "hello"}
	var updated = DELETE(2, init)
	return updated[1]
}

func DELETE[K comparable, V any](key K, oldMap map[K]V) map[K]V {
	var newMap = make(map[K]V)
	for k, v := range oldMap {
		newMap[k] = v
	}
	delete(newMap, key)
	return newMap
}
