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
	var init = map[int]string{0: "hi", 1: "hey"}
	var updated = INSERT(2, "hello", init)
	return updated[2]
}

func INSERT[K comparable, V any](key K, value V, oldMap map[K]V) map[K]V {
	var newMap = make(map[K]V)
	for k, v := range oldMap {
		newMap[k] = v
	}
	newMap[key] = value
	return newMap
}
