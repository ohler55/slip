// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestComputeApplicableMethods(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defgeneric quux (a b)
                   (:method ((a fixnum) (b real) (+ a b)))
                   (:method :before ((a real) (b real) (print a)))
                   (:method :after ((a real) (b real) (print b)))
                   (:method :around ((a fixnum) (b fixnum) (call-next-method a (1+ b)))))`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(compute-applicable-methods 'quux '(1 2))`,
		Validate: func(t *testing.T, v slip.Object) {
			list, _ := v.(slip.List)
			tt.Equal(t, 4, len(list))
			tt.Equal(t, true, strings.Contains(list[0].String(), ":around"))
			tt.Equal(t, true, strings.Contains(list[1].String(), ":before"))
			tt.Equal(t, true, strings.Contains(list[2].String(), "method quux ((a fixnum)"))
			tt.Equal(t, true, strings.Contains(list[3].String(), ":after"))
		},
	}).Test(t)
	(&sliptest.Function{
		Source:    `(compute-applicable-methods 'quux '(1))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestComputeApplicableMethodsNil(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defgeneric quux (a b)
                   (:method ((a fixnum) (b real) (+ a b)))
                   (:method :before ((a real) (b t) (print a)))
                   (:method :after ((a real) (b t) (print b))))`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(compute-applicable-methods 'quux '(1 nil))`,
		Validate: func(t *testing.T, v slip.Object) {
			list, _ := v.(slip.List)
			tt.Equal(t, 2, len(list))
			tt.Equal(t, true, strings.Contains(list[0].String(), ":before"))
			tt.Equal(t, true, strings.Contains(list[1].String(), ":after"))
		},
	}).Test(t)
}

func TestComputeApplicableMethodsNotGeneric(t *testing.T) {
	(&sliptest.Function{
		Source:    `(compute-applicable-methods 'defmethod '(1 2))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
