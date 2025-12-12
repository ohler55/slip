// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUnreadCharStream(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader("abc"))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(progn (read-char in) (unread-char #\x in) (read-char in))`,
		Expect: `#\x`,
	}).Test(t)
}

func TestUnreadCharNotChar(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader("abc"))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(progn (read-char in) (unread-char t in) (read-char in))`,
		Panics: true,
	}).Test(t)
}

func TestUnreadCharNotStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(unread-char #\x t)`,
		Panics: true,
	}).Test(t)
}
