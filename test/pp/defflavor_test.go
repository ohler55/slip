// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp_test

import (
	"fmt"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefflavorBasic(t *testing.T) {
	defer undefFlavor("strawberry")

	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor strawberry ((size "medium")) ()
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables
 (:documentation "Strawberry icecream"))
`, scope).Eval(scope, nil)

	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 60)
                       (f (find-flavor 'strawberry)))
                   (pretty-print f nil))`,
		Expect: `"(defflavor strawberry ((size "medium"))
                      ()
  :inittable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables
  (:documentation "Strawberry icecream"))
"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30))
                   (pretty-print strawberry nil))`,
		Expect: `"(defflavor strawberry
    ((size "medium"))
    ()
  :inittable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables
  (:documentation
   "Strawberry icecream"))
"`,
	}).Test(t)
}

func TestDefflavorEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 30))
                   (pretty-print (defflavor quux ()) nil))`,
		Expect: `"(defflavor quux ()
                ())
"`,
	}).Test(t)
}

func TestDefflavorLongerLines(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 25))
                   (pretty-print (defflavor quux (a) () :inittable-instance-variables) nil))`,
		Expect: `"(defflavor quux (a)
                ()
  :inittable-instance-variables)
"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*print-right-margin* 25))
                   (pretty-print (defflavor quux (a) (some-flavor)) nil))`,
		Expect: `"(defflavor quux (a)
                (some-flavor))
"`,
	}).Test(t)
}

func undefFlavor(fn string) {
	defer func() { _ = recover() }()
	scope := slip.NewScope()
	slip.ReadString(fmt.Sprintf("(undefflavor '%s)", fn), scope).Eval(scope, nil)
}
