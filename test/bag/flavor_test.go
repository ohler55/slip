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
	_ = slip.ReadString("(setq bag (make-instance 'bag-flavor))").Eval(scope, nil)

	// Verify bag-flavor inherits from vanilla-flavor
	result := slip.ReadString("(send bag :operation-handled-p :describe)").Eval(scope, nil)
	tt.Equal(t, slip.True, result)
}

func TestBagFlavorInit(t *testing.T) {
	scope := slip.NewScope()

	obj := slip.ReadString(`(setq bag (make-instance 'bag-flavor :set 7))`).Eval(scope, nil).(*flavors.Instance)
	tt.Equal(t, "7", pretty.SEN(obj.Any))

	obj = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "[1 2 3]"))`).Eval(scope, nil).(*flavors.Instance)
	tt.Equal(t, "[1 2 3]", pretty.SEN(obj.Any))

	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'bag-flavor :parse t)`).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'bag-flavor :set 7 :parse "[]")`).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'bag-flavor :bad 7)`).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method bag-flavor :init out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, ":set"))
	tt.Equal(t, true, strings.Contains(str, ":parse"))
}

func TestBagFlavorInitParseTime(t *testing.T) {
	scope := slip.NewScope()

	_ = slip.ReadString(`(setq *bag-time-format* *rfc3339nano*)`).Eval(scope, nil)
	obj := slip.ReadString(`
(setq bag (make-instance 'bag-flavor :parse "[\"2022-09-19T01:02:03Z\"]"))`).Eval(scope, nil).(*flavors.Instance)
	tt.Equal(t, "[1663549323.000000000]", pretty.SEN(obj.Any, &ojg.Options{TimeFormat: "second"}))

}

// Tested more heavily in the bag-set tests.
func TestBagFlavorSet(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString("(setq bag (make-instance 'bag-flavor))").Eval(scope, nil).(*flavors.Instance)

	_ = slip.ReadString("(send bag :set 7)").Eval(scope, nil)
	tt.Equal(t, "7", pretty.SEN(obj.Any))

	tt.Panic(t, func() { slip.ReadString("(send bag :set 7 t)").Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString("(send bag :set 7 nil t)").Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :set out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-parse tests.
func TestBagFlavorParse(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString("(setq bag (make-instance 'bag-flavor))").Eval(scope, nil).(*flavors.Instance)

	_ = slip.ReadString(`(send bag :parse "{a:3}")`).Eval(scope, nil)
	tt.Equal(t, "{a: 3}", pretty.SEN(obj.Any))

	tt.Panic(t, func() { slip.ReadString(`(send bag :parse "7" t)`).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :parse "7" nil t)`).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :parse out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-get tests.
func TestBagFlavorGet(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a: 7}"))`).Eval(scope, nil)

	result := slip.ReadString(`(send bag :get "a" t)`).Eval(scope, nil)
	tt.Equal(t, "7", pretty.SEN(result))

	result = slip.ReadString(`(send bag :get "a")`).Eval(scope, nil)
	tt.Equal(t, "7", pretty.SEN(result.(*flavors.Instance).Any))

	result = slip.ReadString(`(send bag :get)`).Eval(scope, nil)
	tt.Equal(t, "{a: 7}", pretty.SEN(result.(*flavors.Instance).Any))

	tt.Panic(t, func() { slip.ReadString(`(send bag :get t t)`).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :get "a" nil 7)`).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :get out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-has tests.
func TestBagFlavorHas(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a: 7}"))`).Eval(scope, nil)

	result := slip.ReadString(`(send bag :has "a")`).Eval(scope, nil)
	tt.Equal(t, "t", slip.ObjectString(result))

	tt.Panic(t, func() { slip.ReadString(`(send bag :has t)`).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :has "a" t)`).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :has out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-remove tests.
func TestBagFlavorRemove(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a: 7 b: 8}"))`).Eval(scope, nil).(*flavors.Instance)

	_ = slip.ReadString(`(send bag :remove "a")`).Eval(scope, nil)
	tt.Equal(t, "{b: 8}", pretty.SEN(obj.Any))

	tt.Panic(t, func() { slip.ReadString(`(send bag :remove t)`).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :remove "a" t)`).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :remove out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-remove tests.
func TestBagFlavorModify(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a: 7 b: [1 2 3]}"))`).Eval(scope, nil).(*flavors.Instance)

	_ = slip.ReadString(`(send bag :modify 'reverse "b" :as-bag nil)`).Eval(scope, nil)
	tt.Equal(t, "{a: 7 b: [3 2 1]}", pretty.SEN(obj.Any))

	tt.Panic(t, func() { slip.ReadString(`(send bag :modify t)`).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :modify t "a")`).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :modify 'reverse t)`).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString(`(send bag :modify 'reverse "a" :as-bag)`).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :modify out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-native tests.
func TestBagFlavorNative(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a: 7}"))`).Eval(scope, nil)

	result := slip.ReadString(`(send bag :native)`).Eval(scope, nil)
	tt.Equal(t, `(("a" . 7))`, slip.ObjectString(result))

	tt.Panic(t, func() { slip.ReadString(`(send bag :native t)`).Eval(scope, nil) })

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :native out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-write tests.
func TestBagFlavorWrite(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a: 7}"))`).Eval(scope, nil)

	result := slip.ReadString(`(send bag :write)`).Eval(scope, nil)
	tt.Equal(t, `"{a: 7}"`, slip.ObjectString(result))

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :write out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}

// Tested more heavily in the bag-walk tests.
func TestBagFlavorWalk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq result '())`).Eval(scope, nil)
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "[1 2]"))`).Eval(scope, nil)

	_ = slip.ReadString(`(send bag :walk (lambda (x) (setq result (cons x result))) "*" t)`).Eval(scope, nil)
	tt.Equal(t, "(2 1)", scope.Get(slip.Symbol("result")).String())

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(describe-method bag-flavor :walk out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, "bag-flavor"))
	tt.Equal(t, true, strings.Contains(str, "is a method of"))
}
