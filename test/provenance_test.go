// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"testing"
	"unsafe"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestProvSetBasic(t *testing.T) {
	ps := slip.ProvSet{}
	plus := slip.Symbol("+")
	a := slip.List{plus, slip.Fixnum(2), slip.Fixnum(3)}
	b := slip.List{plus, slip.Fixnum(4), slip.Fixnum(5)}
	c := slip.List{slip.Symbol("*"), slip.Fixnum(6), slip.Fixnum(7)}
	var nilList slip.List

	tt.Nil(t, ps.Get(a))

	ps.Add(a, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   1,
		LastLine:    1,
		FirstColumn: 1,
		LastColumn:  8,
	})
	tt.Nil(t, ps.Get(b))

	ps.Add(b, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   2,
		LastLine:    2,
		FirstColumn: 1,
		LastColumn:  8,
	})
	ps.Add(c, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   3,
		LastLine:    3,
		FirstColumn: 1,
		LastColumn:  8,
	})
	ps.Add(nilList, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   4,
		LastLine:    4,
		FirstColumn: 1,
		LastColumn:  3,
	})

	pa := ps.Get(a)
	tt.NotNil(t, pa)
	tt.Equal(t, 1, pa.FirstLine)

	pb := ps.Get(b)
	tt.NotNil(t, pb)
	tt.Equal(t, 2, pb.FirstLine)

	pc := ps.Get(c)
	tt.NotNil(t, pc)
	tt.Equal(t, 3, pc.FirstLine)

	pn := ps.Get(nilList)
	tt.NotNil(t, pn)
	tt.Equal(t, 4, pn.FirstLine)
}

func TestProvSetSimplify(t *testing.T) {
	ps := slip.ProvSet{}
	a := slip.List{slip.Symbol("+"), slip.Fixnum(2), slip.Fixnum(3)}

	ps.Add(a, &slip.Prov{
		Filepath:    "simple.lisp",
		FirstLine:   1,
		LastLine:    1,
		FirstColumn: 1,
		LastColumn:  8,
	})
	tt.Equal(t, fmt.Sprintf(`[
  {
    key: %d
    value: {
      count: 0
      filepath: simple.lisp
      firstColumn: 1
      firstLine: 1
      lastColumn: 8
      lastLine: 1
    }
  }
]`, uint64(uintptr(unsafe.Pointer(unsafe.SliceData(a))))), ps.String())

}
