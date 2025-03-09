// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestBagReadWithStringPath(t *testing.T) {
	r := strings.NewReader("{a:7}")
	scope := slip.NewScope()
	scope.Let(slip.Symbol("input"), &slip.InputStream{Reader: r})

	obj := slip.ReadString(
		`(setq bag (make-instance bag-flavor :read input))`, scope).Eval(scope, nil).(*flavors.Instance)
	r = strings.NewReader("{b:3}")
	scope.Let(slip.Symbol("input"), &slip.InputStream{Reader: r})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-read bag input "a")`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: {b: 3}}", pretty.SEN(obj.Any))
}

func TestBagReadWithPath(t *testing.T) {
	r := strings.NewReader("{a:7}")
	scope := slip.NewScope()
	scope.Let(slip.Symbol("input"), &slip.InputStream{Reader: r})

	obj := slip.ReadString(
		`(setq bag (make-instance bag-flavor :read input))`, scope).Eval(scope, nil).(*flavors.Instance)
	r = strings.NewReader("{b:3}")
	scope.Let(slip.Symbol("input"), &slip.InputStream{Reader: r})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-read bag input (make-bag-path "a"))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: {b: 3}}", pretty.SEN(obj.Any))
}

func TestBagReadNoPath(t *testing.T) {
	r := strings.NewReader("{a:7}")
	scope := slip.NewScope()
	scope.Let(slip.Symbol("input"), &slip.InputStream{Reader: r})

	obj := slip.ReadString(
		`(setq bag (make-instance bag-flavor :parse "{b:3}"))`, scope).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-read bag input)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 7}", pretty.SEN(obj.Any))
}

func TestBagReadNotStream(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(
		`(setq bag (make-instance bag-flavor :parse "{b:3}"))`, scope).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-read bag t)`,
		Panics: true,
	}).Test(t)
}

func TestBagReadNotBag(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(
		`(setq bag (make-instance bag-flavor :parse "{b:3}"))`, scope).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-read (make-instance vanilla-flavor) t)`,
		Panics: true,
	}).Test(t)
}
