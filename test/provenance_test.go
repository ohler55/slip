// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/slip"
)

func TestProvSetAddList(t *testing.T) {
	var ps slip.ProvSet
	plus := slip.Symbol("+")
	a := slip.List{plus, slip.Fixnum(2), slip.Fixnum(3)}
	b := slip.List{plus, slip.Fixnum(4), slip.Fixnum(5)}
	c := slip.List{slip.Symbol("*"), slip.Fixnum(6), slip.Fixnum(7)}

	ps = ps.Add(a, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   1,
		LastLine:    1,
		FirstColumn: 1,
		LastColumn:  8,
	})
	ps = ps.Add(b, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   2,
		LastLine:    2,
		FirstColumn: 1,
		LastColumn:  8,
	})
	ps = ps.Add(c, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   3,
		LastLine:    3,
		FirstColumn: 1,
		LastColumn:  8,
	})
	ps.Sort()

	fmt.Printf("*** %s\n", pretty.SEN(ps))
	// fmt.Printf("*** a: %d\n", uint64(uintptr(unsafe.Pointer(&a[0]))))
	// fmt.Printf("*** reflect a: %d\n", reflect.ValueOf(a).Pointer())
	// fmt.Printf("*** b: %d\n", uint64(uintptr(unsafe.Pointer(&b[0]))))
	// fmt.Printf("*** reflect b: %d\n", reflect.ValueOf(b).Pointer())
	// fmt.Printf("*** c: %d\n", uint64(uintptr(unsafe.Pointer(&c[0]))))
	// fmt.Printf("*** reflect c: %d\n", reflect.ValueOf(c).Pointer())
	fmt.Printf("*** get a: %s\n", pretty.SEN(ps.Get(a)))
	fmt.Printf("*** get b: %s\n", pretty.SEN(ps.Get(b)))
	fmt.Printf("*** get c: %s\n", pretty.SEN(ps.Get(c)))

}
