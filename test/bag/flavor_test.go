// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/flavors"
)

func TestBagFlavorBasic(t *testing.T) {
	tt.NotNil(t, bag.Flavor())
	scope := slip.NewScope()
	_ = slip.ReadString("(setq bag (make-instance 'bag-flavor))", scope).Eval(scope, nil)

	// Verify bag-flavor inherits from vanilla-flavor
	result := slip.ReadString("(send bag :operation-handled-p :describe)", scope).Eval(scope, nil)
	tt.Equal(t, slip.True, result)
}

func TestBagFlavorInit(t *testing.T) {
	scope := slip.NewScope()

	obj := slip.ReadString(`(setq bag (make-instance 'bag-flavor :set 7))`, scope).Eval(scope, nil).(*flavors.Instance)
	tt.Equal(t, "7", pretty.SEN(obj.Any))

	obj = slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "[1 2 3]"))`, scope).Eval(scope, nil).(*flavors.Instance)
	tt.Equal(t, "[1 2 3]", pretty.SEN(obj.Any))

	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'bag-flavor :parse t)`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'bag-flavor :set 7 :parse "[]")`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'bag-flavor :bad 7)`, scope).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method bag-flavor :init out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, ":set"))
	tt.Equal(t, true, strings.Contains(str, ":parse"))
}

func TestBagFlavorInitParseTime(t *testing.T) {
	scope := slip.NewScope()

	_ = slip.ReadString(`(setq *bag-time-format* *rfc3339nano*)`, scope).Eval(scope, nil)
	obj := slip.ReadString(`
(setq bag (make-instance 'bag-flavor :parse "[\"2022-09-19T01:02:03Z\"]"))`, scope).Eval(scope, nil).(*flavors.Instance)
	tt.Equal(t, "[1663549323.000000000]", pretty.SEN(obj.Any, &ojg.Options{TimeFormat: "second"}))
}

func TestBagFlavorInitReadTime(t *testing.T) {
	r := strings.NewReader("[\"2022-09-19T01:02:03Z\"]")
	scope := slip.NewScope()
	scope.Let(slip.Symbol("input"), slip.NewInputStream(r))
	obj := slip.ReadString(`
(setq bag (make-instance 'bag-flavor :read input))`, scope).Eval(scope, nil).(*flavors.Instance)
	tt.Equal(t, "[1663549323.000000000]", pretty.SEN(obj.Any, &ojg.Options{TimeFormat: "second"}))

	tt.Panic(t, func() { slip.ReadString("(make-instance bag-flavor :read t)", scope).Eval(scope, nil) })
}

// Tested more heavily in the bag-set tests.
func TestBagFlavorSet(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString("(setq bag (make-instance 'bag-flavor))", scope).Eval(scope, nil).(*flavors.Instance)

	_ = slip.ReadString("(send bag :set 7)", scope).Eval(scope, nil)
	tt.Equal(t, "7", pretty.SEN(obj.Any))

	tt.Panic(t, func() { slip.ReadString("(send bag :set 7 t)", scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString("(send bag :set 7 nil t)", scope).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :set out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-parse tests.
func TestBagFlavorParse(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString("(setq bag (make-instance 'bag-flavor))", scope).Eval(scope, nil).(*flavors.Instance)

	_ = slip.ReadString(`(send bag :parse "{a:3}")`, scope).Eval(scope, nil)
	tt.Equal(t, "{a: 3}", pretty.SEN(obj.Any))

	tt.Panic(t, func() { slip.ReadString(`(send bag :parse "7" t)`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :parse "7" nil t)`, scope).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :parse out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-read tests.
func TestBagFlavorRead(t *testing.T) {
	r := strings.NewReader("{a:3}")
	scope := slip.NewScope()
	obj := slip.ReadString("(setq bag (make-instance 'bag-flavor))", scope).Eval(scope, nil).(*flavors.Instance)
	scope.Let(slip.Symbol("input"), slip.NewInputStream(r))
	_ = slip.ReadString(`(send bag :read input)`, scope).Eval(scope, nil)
	tt.Equal(t, "{a: 3}", pretty.SEN(obj.Any))

	tt.Panic(t, func() { slip.ReadString(`(send bag :read input t)`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :read input nil t)`, scope).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :read out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-get tests.
func TestBagFlavorGet(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a: 7}"))`, scope).Eval(scope, nil)

	result := slip.ReadString(`(send bag :get "a")`, scope).Eval(scope, nil)
	tt.Equal(t, "7", pretty.SEN(result))

	result = slip.ReadString(`(send bag :get "a" t)`, scope).Eval(scope, nil)
	tt.Equal(t, "7", pretty.SEN(result.(*flavors.Instance).Any))

	result = slip.ReadString(`(send bag :get)`, scope).Eval(scope, nil)
	tt.Equal(t, `(("a" . 7))`, slip.ObjectString(result))

	tt.Panic(t, func() { slip.ReadString(`(send bag :get t t)`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :get "a" nil 7)`, scope).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :get out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-has tests.
func TestBagFlavorHas(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a: 7}"))`, scope).Eval(scope, nil)

	result := slip.ReadString(`(send bag :has "a")`, scope).Eval(scope, nil)
	tt.Equal(t, "t", slip.ObjectString(result))

	tt.Panic(t, func() { slip.ReadString(`(send bag :has t)`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :has "a" t)`, scope).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :has out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-remove tests.
func TestBagFlavorRemove(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a: 7 b: 8}"))`, scope).Eval(scope, nil).(*flavors.Instance)

	_ = slip.ReadString(`(send bag :remove "a")`, scope).Eval(scope, nil)
	tt.Equal(t, "{b: 8}", pretty.SEN(obj.Any))

	tt.Panic(t, func() { slip.ReadString(`(send bag :remove t)`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :remove "a" t)`, scope).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :remove out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-remove tests.
func TestBagFlavorModify(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a: 7 b: [1 2 3]}"))`, scope).Eval(scope, nil).(*flavors.Instance)

	_ = slip.ReadString(`(send bag :modify 'reverse "b" :as-bag nil)`, scope).Eval(scope, nil)
	tt.Equal(t, "{a: 7 b: [3 2 1]}", pretty.SEN(obj.Any))

	tt.Panic(t, func() { slip.ReadString(`(send bag :modify t)`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :modify t "a")`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :modify 'reverse t)`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :modify 'reverse "a" :as-bag)`, scope).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :modify out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-native tests.
func TestBagFlavorNative(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a: 7}"))`, scope).Eval(scope, nil)

	result := slip.ReadString(`(send bag :native)`, scope).Eval(scope, nil)
	tt.Equal(t, `(("a" . 7))`, slip.ObjectString(result))

	tt.Panic(t, func() { slip.ReadString(`(send bag :native t)`, scope).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :native out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-write tests.
func TestBagFlavorWrite(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a: 7}"))`, scope).Eval(scope, nil)

	result := slip.ReadString(`(send bag :write)`, scope).Eval(scope, nil)
	tt.Equal(t, `"{a: 7}"`, slip.ObjectString(result))

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :write out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-walk tests.
func TestBagFlavorWalk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq result '())`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "[1 2]"))`, scope).Eval(scope, nil)

	_ = slip.ReadString(`(send bag :walk (lambda (x) (setq result (cons x result))) "*")`, scope).Eval(scope, nil)
	tt.Equal(t, "(2 1)", scope.Get(slip.Symbol("result")).String())

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :walk out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}
