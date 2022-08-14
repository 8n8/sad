package main

import (
	"fmt"
)

func main() {
	fmt.Println(sumOfSlice([]int{3}))
}

type Increment struct {
	sum   int
	i     int
	slice []int
}

func sumOfSlice(slice []int) int {
	return LOOP(
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
		}).sum
}

func LOOP[T any](mutable T, update func(T) T, keepGoing func(T) bool) T {
	for keepGoing(mutable) {
		mutable = update(mutable)
	}
	return mutable
}
