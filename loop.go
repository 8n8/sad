package main

import (
	"fmt"
)

func main() {
	fmtPrintln(fmt.Sprintf("%d", sumOfSlice([]int{1, 2, 3}))).RUN()
}

type fmtPrintln string

func (p fmtPrintln) RUN() {
	fmt.Println(string(p))
}

type Increment struct {
	sum   int
	i     int
	slice []int
}

func sumOfSlice(slice []int) int {
	var result Increment = LOOP(
		Increment{sum: 0, i: 0, slice: slice},
		func(x Increment) Increment {
			return Increment{
				sum:   x.sum + x.slice[x.i],
				i:     x.i + 1,
				slice: slice,
			}
		},
		func(x Increment) bool {
			return x.i != len(x.slice)
		})
	return result.sum
}

func LOOP[T any](mutable T, update func(T) T, keepGoing func(T) bool) T {
	for keepGoing(mutable) {
		mutable = update(mutable)
	}
	return mutable
}
