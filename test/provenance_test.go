// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"testing"
	"unsafe"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestProvenanceGlobal(t *testing.T) {
	slip.ResetProvenance()

	plus := slip.Symbol("+")
	a := slip.List{plus, slip.Fixnum(2), slip.Fixnum(3)}
	b := slip.List{plus, slip.Fixnum(4), slip.Fixnum(5)}
	c := slip.List{slip.Symbol("*"), slip.Fixnum(6), slip.Fixnum(7)}
	var nilList slip.List

	tt.Nil(t, slip.GetProvenance(a))
	slip.AddProvenance(a, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   1,
		LastLine:    1,
		FirstColumn: 1,
		LastColumn:  8,
	})

	tt.Nil(t, slip.GetProvenance(b))
	slip.AddProvenance(b, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   2,
		LastLine:    2,
		FirstColumn: 1,
		LastColumn:  8,
	})
	slip.AddProvenance(c, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   3,
		LastLine:    3,
		FirstColumn: 1,
		LastColumn:  8,
	})
	slip.AddProvenance(nilList, &slip.Prov{
		Filepath:    "a-file",
		FirstLine:   4,
		LastLine:    4,
		FirstColumn: 1,
		LastColumn:  3,
	})

	pa := slip.GetProvenance(a)
	tt.NotNil(t, pa)
	tt.Equal(t, 1, pa.FirstLine)

	pb := slip.GetProvenance(b)
	tt.NotNil(t, pb)
	tt.Equal(t, 2, pb.FirstLine)

	pc := slip.GetProvenance(c)
	tt.NotNil(t, pc)
	tt.Equal(t, 3, pc.FirstLine)

	pn := slip.GetProvenance(nilList)
	tt.NotNil(t, pn)
	tt.Equal(t, 4, pn.FirstLine)

	simple := slip.DebugProvenance()
	var prev int64
	for _, key := range jp.MustParseString("[*].key").Get(simple) {
		tt.Equal(t, true, prev <= key.(int64))
		prev = key.(int64)
	}
}

func TestProvenanceSimplify(t *testing.T) {
	slip.ResetProvenance()
	a := slip.List{slip.Symbol("+"), slip.Fixnum(2), slip.Fixnum(3)}
	slip.AddProvenance(a, &slip.Prov{
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
]`, uint64(uintptr(unsafe.Pointer(unsafe.SliceData(a))))), pretty.SEN(slip.DebugProvenance()))

	scope := slip.NewScope()
	defer func() {
		slip.ResetCoverage(true)
		slip.StopCoverage()
	}()
	slip.ResetCoverage(true)
	slip.StartCoverage()
	_ = a.Eval(scope, 0)
	tt.Equal(t, fmt.Sprintf(`[
  {
    key: %d
    value: {
      count: 1
      filepath: simple.lisp
      firstColumn: 1
      firstLine: 1
      lastColumn: 8
      lastLine: 1
    }
  }
]`, uint64(uintptr(unsafe.Pointer(unsafe.SliceData(a))))), pretty.SEN(slip.DebugProvenance()))

	slip.ResetProvenanceUsage()
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
]`, uint64(uintptr(unsafe.Pointer(unsafe.SliceData(a))))), pretty.SEN(slip.DebugProvenance()))
}
