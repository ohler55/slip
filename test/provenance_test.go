// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestProvSetBasic(t *testing.T) {
	var ps slip.ProvSet
	plus := slip.Symbol("+")
	a := slip.List{plus, slip.Fixnum(2), slip.Fixnum(3)}
	b := slip.List{plus, slip.Fixnum(4), slip.Fixnum(5)}
	c := slip.List{slip.Symbol("*"), slip.Fixnum(6), slip.Fixnum(7)}
	var nilList slip.List

	tt.Nil(t, ps.Get(a))

	ps = ps.Add(a, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   1,
		LastLine:    1,
		FirstColumn: 1,
		LastColumn:  8,
	})
	tt.Nil(t, ps.Get(b))

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
	ps = ps.Add(nilList, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   4,
		LastLine:    4,
		FirstColumn: 1,
		LastColumn:  3,
	})

	ps.Sort()

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

func TestProvSetEdges(t *testing.T) {
	var ps slip.ProvSet

	for i := uint64(1); i < 10; i++ {
		if i == 5 {
			continue
		}
		ps = ps.AddByKey(i, &slip.Prov{
			FirstLine:   uint32(i),
			LastLine:    uint32(i),
			FirstColumn: 1,
			LastColumn:  5,
		})
	}
	ps.Sort()

	for i := uint64(1); i < 10; i++ {
		if i == 5 {
			continue
		}
		p := ps.GetByKey(i)
		tt.NotNil(t, p)
		tt.Equal(t, uint32(i), p.FirstLine)
	}
	tt.Nil(t, ps.GetByKey(0))
	tt.Nil(t, ps.GetByKey(5))
	tt.Nil(t, ps.GetByKey(100))
}

func TestProvSetSimplify(t *testing.T) {
	var ps slip.ProvSet

	for i := uint64(1); i < 3; i++ {
		ps = ps.AddByKey(i, &slip.Prov{
			Filepath:    "simple.lisp",
			FirstLine:   uint32(i),
			LastLine:    uint32(i),
			FirstColumn: 1,
			LastColumn:  5,
		})
	}
	ps.Sort()
	tt.Equal(t, `[
  {
    key: 1
    value: {
      count: 0
      filepath: simple.lisp
      firstColumn: 1
      firstLine: 1
      lastColumn: 5
      lastLine: 1
    }
  }
  {
    key: 2
    value: {
      count: 0
      filepath: simple.lisp
      firstColumn: 1
      firstLine: 2
      lastColumn: 5
      lastLine: 2
    }
  }
]`, pretty.SEN(ps))
}
