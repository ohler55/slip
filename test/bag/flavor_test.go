// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/flavors"
)

func TestBagFlavorBasic(t *testing.T) {
	tt.NotNil(t, bag.Flavor())
	scope := slip.NewScope()
	_ = slip.ReadString("(setq bag (make-instance 'bag-flavor))").Eval(scope)

	// Verify bag-flavor inherits from vanilla-flavor
	result := slip.ReadString("(send bag :operation-handler-p :describe)").Eval(scope)
	tt.Equal(t, slip.True, result)
}

func TestBagFlavorInit(t *testing.T) {
	scope := slip.NewScope()

	obj := slip.ReadString(`(setq bag (make-instance 'bag-flavor :set 7))`).Eval(scope).(*flavors.Instance)
	tt.Equal(t, "7", pretty.SEN(obj.Any))

	obj = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "[1 2 3]"))`).Eval(scope).(*flavors.Instance)
	tt.Equal(t, "[1 2 3]", pretty.SEN(obj.Any))

	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'bag-flavor :parse t)`).Eval(scope) })
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'bag-flavor :set 7 :parse "[]")`).Eval(scope) })
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'bag-flavor :bad 7)`).Eval(scope) })
}

// Tested more heavily in the bag-set tests.
func TestBagFlavorSet(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString("(setq bag (make-instance 'bag-flavor))").Eval(scope).(*flavors.Instance)

	_ = slip.ReadString("(send bag :set nil 7)").Eval(scope)
	tt.Equal(t, "7", pretty.SEN(obj.Any))

	tt.Panic(t, func() { slip.ReadString("(send bag :set nil)").Eval(scope) })
}

// TBD test each method
