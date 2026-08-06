// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"fmt"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRecoverCatch(t *testing.T) {
	(&sliptest.Function{
		Source: `(recover rec rec 'abc (panic 'catch-me) 'def)`,
		Expect: "catch-me",
	}).Test(t)
}

func TestRecoverOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(recover rec rec 'abc 'def)`,
		Expect: "def",
	}).Test(t)
}

func TestRecoverArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(recover x)`,
		Panics: true,
	}).Test(t)
}

func TestRecoverSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(recover t 7)`,
		Panics: true,
	}).Test(t)
}

func TestRecoverPreProv(t *testing.T) {
	orig := slip.Provenance
	slip.Provenance = true
	defer func() { slip.Provenance = orig }()
	(&sliptest.Function{
		Source: `(recover rec (list rec) 'abc (panic 'catch-me) 'def)`,
		Expect: "(catch-me)",
	}).Test(t)
}

func undefFlavor(fn string) {
	defer func() { _ = recover() }()
	scope := slip.NewScope()
	slip.ReadString(fmt.Sprintf("(undefflavor '%s)", fn), scope).Eval(scope, nil)
}

func TestRecoverMethod(t *testing.T) {
	defer undefFlavor("blueberry")
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor blueberry ((size "medium")) ())
(defmethod (blueberry :quux) ()
  (let ((s self))
    (recover r nil (panic "fake error"))
    (equal s self)))`, scope).Eval(scope, nil)

	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((berry (make-instance 'blueberry)))
                   (send berry :quux))`,
		Expect: "t",
	}).Test(t)
}
