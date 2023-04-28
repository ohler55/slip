// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAppendEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(append)`,
		Expect: "nil",
	}).Test(t)
}

func TestAppendObject(t *testing.T) {
	(&sliptest.Function{
		Source: `(append 'a)`,
		Expect: "a",
	}).Test(t)
}

func TestAppendEmptyLisy(t *testing.T) {
	(&sliptest.Function{
		Source: `(append '() '(a b))`,
		Expect: "(a b)",
	}).Test(t)
}

func TestAppendListList(t *testing.T) {
	(&sliptest.Function{
		Source: `(append '(a b) '(c d) nil '() '(e f))`,
		Expect: "(a b c d e f)",
	}).Test(t)
}

func TestAppendListCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(append '(a b) '(c . d))`,
		Expect: "(a b c . d)",
	}).Test(t)
}

func TestAppendListObject(t *testing.T) {
	(&sliptest.Function{
		Source: `(append '(a b) 'c)`,
		Expect: "(a b . c)",
	}).Test(t)
}

func TestAppendConsList(t *testing.T) {
	(&sliptest.Function{
		Source: `(append '(a . b) '(c d))`,
		Panics: true,
	}).Test(t)
}

func TestAppendConsCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(append '(a . b) '(c . d))`,
		Panics: true,
	}).Test(t)
}

func TestAppendListNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(append 'a '(c d))`,
		Panics: true,
	}).Test(t)
}

func TestAppendConsObject(t *testing.T) {
	(&sliptest.Function{
		Source: `(append '(a . b) 'c)`,
		Panics: true,
	}).Test(t)
}
