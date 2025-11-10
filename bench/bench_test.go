package bench_test

import "testing"

type Cons struct {
	car any
	cdr any
}

func BenchmarkCons(b *testing.B) {
	var list *Cons

	for n := 0; n < b.N; n++ {
		c := &Cons{car: n, cdr: list}
		list = c
	}
}

func BenchmarkSlice(b *testing.B) {
	var list []any

	for n := 0; n < b.N; n++ {
		list = append(list, n)
	}
	if len(list) == 0 {
		b.Failed()
	}
}

func BenchmarkSlice2(b *testing.B) {
	list := make([]any, 0, b.N)
	for n := 0; n < b.N; n++ {
		list = append(list, n)
	}
	if len(list) == 0 {
		b.Failed()
	}
}

func BenchmarkMapcarCons(b *testing.B) {
	var list *Cons
	for n := 0; n < b.N; n++ {
		c := &Cons{car: n, cdr: list}
		list = c
	}
	b.ResetTimer()
	var dup *Cons
	for c := list; c != nil; c = c.cdr.(*Cons) {
		d := &Cons{car: c.car, cdr: dup}
		dup = d
	}
}

func BenchmarkMapcarSlice(b *testing.B) {
	var list []any

	for n := 0; n < b.N; n++ {
		list = append(list, n)
	}
	b.ResetTimer()
	dup := make([]any, len(list))
	copy(dup, list)
}

/*
func BenchmarkMapcCons(b *testing.B) {
	var list *Cons
	for n := 0; n < b.N; n++ {
		c := &Cons{car: n, cdr: list}
		list = c
	}
	b.ResetTimer()
	for i := 1_000_000; 0 < i; i-- {
		for c := list; c != nil; c = c.cdr.(*Cons) {
		}
	}
}

func BenchmarkMapcSlice(b *testing.B) {
	var list []any

	for n := 0; n < b.N; n++ {
		list = append(list, n)
	}
	b.ResetTimer()
	for i := 1_000_000; 0 < i; i-- {
		for range list {
		}
	}
}
*/
