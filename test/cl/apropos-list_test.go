// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAproposListString(t *testing.T) {
	(&sliptest.Function{
		Source: `(apropos-list "terpr")`,
		Expect: "(terpri)",
	}).Test(t)
}

func TestAproposListSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(apropos-list 'terpr)`,
		Expect: "(terpri)",
	}).Test(t)
}

func TestAproposListMultiple(t *testing.T) {
	(&sliptest.Function{
		Source: `(apropos-list 'lambda)`,
		Expect: `/\(.*print-lambda.* lambda.*/`,
	}).Test(t)
}

func TestAproposListPkg(t *testing.T) {
	(&sliptest.Function{
		Source: `(apropos-list 'def 'flavors)`,
		Expect: "(defflavor defmethod defwhopper undefflavor)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(apropos-list "vanilla" 'flavors)`,
		Expect: "(vanilla-flavor)",
	}).Test(t)
}

func TestAproposListBadArg(t *testing.T) {
	(&sliptest.Function{
		Source: `(apropos-list)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(apropos-list t)`,
		Panics: true,
	}).Test(t)
}

func TestAproposListBadPkg(t *testing.T) {
	(&sliptest.Function{
		Source: `(apropos-list 'def "boo")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(apropos-list 'def t)`,
		Panics: true,
	}).Test(t)
}
