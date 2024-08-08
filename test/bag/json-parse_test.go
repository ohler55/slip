// Copyright (c) 2022, Peter Ohler, All rights reserved.

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

func TestBagJSONParseStream(t *testing.T) {
	r := strings.NewReader("{a:1}{b:2}{c:3}")
	scope := slip.NewScope()
	scope.Let(slip.Symbol("input"), &slip.InputStream{Reader: r})
	scope.Set(slip.Symbol("json-parse-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(json-parse
                  (lambda (bag) (setq json-parse-test-out (cons (send bag :native) json-parse-test-out)))
                  input)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("json-parse-test-out"))))
}

func TestBagJSONParseStreamStrict(t *testing.T) {
	r := strings.NewReader(`{"a":1}{"b":2}{"c":3}`)
	scope := slip.NewScope()
	scope.Let(slip.Symbol("input"), &slip.InputStream{Reader: r})
	scope.Set(slip.Symbol("json-parse-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(json-parse
                  (lambda (bag) (setq json-parse-test-out (cons (send bag :native) json-parse-test-out)))
                  input t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("json-parse-test-out"))))
}

func TestBagJSONParseString(t *testing.T) {
	scope := slip.NewScope()
	scope.Set(slip.Symbol("json-parse-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(json-parse
                  (lambda (bag) (setq json-parse-test-out (cons (send bag :native) json-parse-test-out)))
                  "{a:1}{b:2}{c:3}")`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("json-parse-test-out"))))
}

func TestBagJSONParseOctets(t *testing.T) {
	scope := slip.NewScope()
	scope.Set(slip.Symbol("json-parse-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(json-parse
                  (lambda (bag) (setq json-parse-test-out (cons (send bag :native) json-parse-test-out)))
                  (coerce "{a:1}{b:2}{c:3}" 'octets))`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("json-parse-test-out"))))
}

func TestBagJSONParseStringStrict(t *testing.T) {
	scope := slip.NewScope()
	scope.Set(slip.Symbol("json-parse-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(json-parse
                  (lambda (bag) (setq json-parse-test-out (cons (send bag :native) json-parse-test-out)))
                  "{\"a\":1}{\"b\":2}{\"c\":3}" t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("json-parse-test-out"))))
}

func TestBagJSONParseOctetsStrict(t *testing.T) {
	scope := slip.NewScope()
	scope.Set(slip.Symbol("json-parse-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(json-parse
                  (lambda (bag) (setq json-parse-test-out (cons (send bag :native) json-parse-test-out)))
                  (coerce "{\"a\":1}{\"b\":2}{\"c\":3}" 'octets) t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`((("c" . 3)) (("b" . 2)) (("a" . 1)))`,
		slip.ObjectString(scope.Get(slip.Symbol("json-parse-test-out"))))
}

func TestBagJSONParseChannel(t *testing.T) {
	scope := slip.NewScope()
	channel := make(gi.Channel, 10)
	scope.Let(slip.Symbol("chan"), channel)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(json-parse chan "{a:1}{b:2}{c:3}")`,
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

func TestBagJSONParseBadInput(t *testing.T) {
	(&sliptest.Function{
		Source:    `(json-parse (lambda (bag) nil) t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(json-parse (lambda (bag) nil) t t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
