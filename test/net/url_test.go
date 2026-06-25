// Copyright (c) 2026, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestURLInit(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance :url-flavor :url "http://localhost:xyz")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance :url-flavor)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestURLScheme(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance :url-flavor :url "http://you:secret@localhost:12345/home?quux=abc") :scheme)`,
		Expect: `"http"`,
	}).Test(t)
}

func TestURLHost(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance :url-flavor :url "http://you:secret@localhost:12345/home?quux=abc") :host)`,
		Expect: `"localhost"`,
	}).Test(t)
}

func TestURLPort(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance :url-flavor :url "http://you:secret@localhost:12345/home?quux=abc") :port)`,
		Expect: `12345`,
	}).Test(t)
}

func TestURLUser(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance :url-flavor :url "http://you:secret@localhost:12345/home?quux=abc") :user)`,
		Expect: `"you"`,
	}).Test(t)
}

func TestURLPassword(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance :url-flavor :url "http://you:secret@localhost:12345/home?quux=abc") :password)`,
		Expect: `"secret"`,
	}).Test(t)
}

func TestURLPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance :url-flavor :url "http://you:secret@localhost:12345/home?quux=abc") :path)`,
		Expect: `"/home"`,
	}).Test(t)
}

func TestURLQuery(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance :url-flavor :url "http://you:secret@localhost:12345/home?quux=abc") :query)`,
		Expect: `(("quux" "abc"))`,
	}).Test(t)
}

func TestURLString(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance :url-flavor :url "http://you:secret@localhost:12345/home?quux=abc") :string)`,
		Expect: `"http://you:secret@localhost:12345/home?quux=abc"`,
	}).Test(t)
}
