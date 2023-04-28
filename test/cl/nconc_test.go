// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNconcEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(nconc)`,
		Expect: "nil",
	}).Test(t)
}

func TestNconcObject(t *testing.T) {
	(&sliptest.Function{
		Source: `(nconc 'a)`,
		Expect: "a",
	}).Test(t)
}

func TestNconcListList(t *testing.T) {
	(&sliptest.Function{
		Source: `(nconc '(a b) '(c d) nil '() '(e f))`,
		Expect: "(a b c d e f)",
	}).Test(t)
}

func TestNconcListCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(nconc '(a b) '(c . d))`,
		Expect: "(a b c . d)",
	}).Test(t)
}

func TestNconcConsList(t *testing.T) {
	(&sliptest.Function{
		Source: `(nconc '(a . b) '(c d))`,
		Expect: "(a c d)",
	}).Test(t)
}

func TestNconcConsCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(nconc '(a . b) '(c . d))`,
		Expect: "(a c . d)",
	}).Test(t)
}

func TestNconcConsObject(t *testing.T) {
	(&sliptest.Function{
		Source: `(nconc '(a . b) 'c)`,
		Expect: "(a . c)",
	}).Test(t)
}

func TestNconcListObject(t *testing.T) {
	(&sliptest.Function{
		Source: `(nconc '(a b) 'c)`,
		Expect: "(a b . c)",
	}).Test(t)
}

func TestNconcListNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(nconc 'a '(c d))`,
		Panics: true,
	}).Test(t)
}
