// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLetEmpty(t *testing.T) {
	scope := slip.NewScope()
	tt.Equal(t, nil, slip.ReadEvalString("(let ())", scope))
}

func TestLetNoInitialForm(t *testing.T) {
	scope := slip.NewScope()
	tt.Equal(t, "(nil nil)", slip.ObjectString(slip.ReadEvalString("(let (x (y)) (list x y))", scope)))
}

func TestLetPreProv(t *testing.T) {
	orig := slip.Provenance
	slip.Provenance = true
	defer func() { slip.Provenance = orig }()
	scope := slip.NewScope()
	tt.Equal(t, "(nil nil)", slip.ObjectString(slip.ReadEvalString("(let (x (y)) (list x y))", scope)))
}

func TestLetNested(t *testing.T) {
	scope := slip.NewScope()
	tt.Equal(t, "(1 2)", slip.ObjectString(slip.ReadEvalString("(let ((x 1)) (let ((y 2)) (list x y)))", scope)))
}

func TestLetParallel(t *testing.T) {
	scope := slip.NewScope()
	tt.Equal(t, "(4 3)", slip.ObjectString(slip.ReadEvalString("(setq x 3) (let ((x 4) (y x)) (list x y))", scope)))
}

func TestLetNoBindings(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(let)", scope) })
}

func TestLetBadBindings(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(let t)", scope) })
}

func TestLetEmptyBinding(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(let (()))", scope) })
}

func TestLetBadBindingVar(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(let ((t 3)))", scope) })
}

func TestLetBadVar(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(let (t))", scope) })
}

func TestLetGo(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (z)
                  (tagbody
                   (let ((x 1) (y 2))
                    (setq z (list x y))
                    (go skip)
                    (setq z 0))
                   skip)
                  z)`,
		Expect: "(1 2)",
	}).Test(t)
}
