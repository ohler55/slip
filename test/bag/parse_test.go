// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/ojg"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestBagParseWithPath(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-parse bag (coerce "{b:3}" 'octets) "a")`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: {b: 3}}", pretty.SEN(obj.Any))
}

func TestBagParseTimeFormat(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq *bag-time-format* *rfc3339nano*)`).Eval(scope, nil)
	obj := slip.ReadString(`(setq bag (make-instance 'bag-flavor))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-parse bag "[\"2022-09-19T01:02:03Z\"]")`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "[1663549323.000000000]", pretty.SEN(obj.Any, &ojg.Options{TimeFormat: "second"}))
}

func TestBagParseSecondPath(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq *bag-time-format* "second")`).Eval(scope, nil)
	obj := slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope, nil).(*flavors.Instance)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-parse bag "1663549323.0" (make-bag-path "a"))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 1663549323000000000}", pretty.SEN(obj.Any))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-parse bag "12345.678" (make-bag-path "a"))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 12345.678}", pretty.SEN(obj.Any))
}

func TestBagParseNanoPath(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq *bag-time-format* 'nano)`).Eval(scope, nil)
	obj := slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope, nil).(*flavors.Instance)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-parse bag "1663549323000000000" (make-bag-path "a"))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 1663549323.000000000}", pretty.SEN(obj.Any, &ojg.Options{TimeFormat: "second"}))
}

func TestBagParseDate(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq *bag-time-format* "2006-01-02")`).Eval(scope, nil)
	obj := slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope, nil).(*flavors.Instance)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-parse bag "\"2022-09-19\"" (make-bag-path "a"))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 1663545600.000000000}", pretty.SEN(obj.Any, &ojg.Options{TimeFormat: "second"}))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-parse bag "\"2022/09/19\"" (make-bag-path "a"))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, `{a: "2022/09/19"}`, pretty.SEN(obj.Any, &ojg.Options{TimeFormat: "second"}))
}

func TestBagParseTimeWrap(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq *bag-time-format* *rfc3339nano* *bag-time-wrap* "tm")`).Eval(scope, nil)
	obj := slip.ReadString(`(setq bag (make-instance 'bag-flavor))`).Eval(scope, nil).(*flavors.Instance)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-parse bag "[{tm: \"2022-09-19T01:02:03Z\"}]")`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "[1663549323.000000000]", pretty.SEN(obj.Any, &ojg.Options{TimeFormat: "second"}))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-parse bag "[{tm: 1663549323000000000}]")`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "[1663549323.000000000]", pretty.SEN(obj.Any, &ojg.Options{TimeFormat: "second"}))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-parse bag "[{tm: true}]")`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "[{tm: true}]", pretty.SEN(obj.Any))
}

func TestBagParseArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-parse (make-instance 'bag-flavor))`,
		Panics: true,
	}).Test(t)
}

func TestBagParseNotBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-parse (make-instance 'vanilla-flavor) "3")`,
		Panics: true,
	}).Test(t)
}

func TestBagParseBadString(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-parse (make-instance 'bag-flavor) t)`,
		Panics: true,
	}).Test(t)
}

func TestBagParseBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-parse (make-instance 'bag-flavor) "7" t)`,
		Panics: true,
	}).Test(t)
}
