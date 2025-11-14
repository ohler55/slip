// Copyright (c) 2025, Peter Ohler, All rights reserved.

package bag_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestBagDiscoverJSONStream(t *testing.T) {
	r := strings.NewReader("{a:1}{b:2}{c:3}")
	scope := slip.NewScope()
	scope.Let(slip.Symbol("input"), &slip.InputStream{Reader: r})
	scope.Set(slip.Symbol("discover-json-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(discover-json
                  (lambda (bag) (setq discover-json-test-out (cons (send bag :native) discover-json-test-out)) nil)
                  input)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("discover-json-test-out"))))
}

func TestBagDiscoverJSONStreamStrict(t *testing.T) {
	r := strings.NewReader(`{"a":1}{"b":2}{"c":3}`)
	scope := slip.NewScope()
	scope.Let(slip.Symbol("input"), &slip.InputStream{Reader: r})
	scope.Set(slip.Symbol("discover-json-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(discover-json
                  (lambda (bag) (setq discover-json-test-out (cons (send bag :native) discover-json-test-out)) nil)
                  input t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("discover-json-test-out"))))
}

func TestBagDiscoverJSONString(t *testing.T) {
	scope := slip.NewScope()
	scope.Set(slip.Symbol("discover-json-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(discover-json
                  (lambda (bag) (setq discover-json-test-out (cons (send bag :native) discover-json-test-out)) nil)
                  "first {a:1} second {b:2} third{c:3}")`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("discover-json-test-out"))))
}

func TestBagDiscoverJSONOctets(t *testing.T) {
	scope := slip.NewScope()
	scope.Set(slip.Symbol("discover-json-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(discover-json
                  (lambda (bag) (setq discover-json-test-out (cons (send bag :native) discover-json-test-out)) nil)
                  (coerce "x {a:1} y {b:2} z {c:3}" 'octets))`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("discover-json-test-out"))))
}

func TestBagDiscoverJSONStringStrict(t *testing.T) {
	scope := slip.NewScope()
	scope.Set(slip.Symbol("discover-json-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(discover-json
                  (lambda (bag) (setq discover-json-test-out (cons (send bag :native) discover-json-test-out)) nil)
                  "x {\"a\":1} y {\"b\":2} z {\"c\":3}" t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("discover-json-test-out"))))
}

func TestBagDiscoverJSONOctetsStrict(t *testing.T) {
	scope := slip.NewScope()
	scope.Set(slip.Symbol("discover-json-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(discover-json
                  (lambda (bag) (setq discover-json-test-out (cons (send bag :native) discover-json-test-out)) nil)
                  (coerce "x{\"a\":1}y{\"b\":2}z{\"c\":3}" 'octets) t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("discover-json-test-out"))))
}

func TestBagDiscoverJSONChannel(t *testing.T) {
	scope := slip.NewScope()
	channel := make(gi.Channel, 10)
	scope.Let(slip.Symbol("chan"), channel)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(discover-json chan "x {a:1} y {b:2} z {c:3}")`,
		Expect: "nil",
	}).Test(t)
	var result slip.List
	for i := 0; i < 3; i++ {
		b := <-channel
		b = b.(*flavors.Instance).Receive(scope, ":native", nil, 0)
		result = append(result, b)
	}
	tt.Equal(t,
		`((("a" . 1)) (("b" . 2)) (("c" . 3)))`,
		slip.ObjectString(result))
}

func TestBagDiscoverJSONBadInput(t *testing.T) {
	(&sliptest.Function{
		Source:    `(discover-json (lambda (bag) nil) t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(discover-json (lambda (bag) nil) t t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
