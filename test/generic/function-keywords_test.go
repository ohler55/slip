// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFunctionKeywordsOkay(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defmethod quux ((a fixnum) &key (b 3) (c 4) d &allow-other-keys) (list a b c d))`,
		Expect: `/#<method quux \(\(a fixnum\) &key \(b 3\) \(c 4\) d &allow-other-keys\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(function-keywords (find-method 'quux '() '(fixnum)))`,
		Expect: `(:b :c :d), t`,
	}).Test(t)
}

func TestFunctionKeywordsNotMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(function-keywords t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
