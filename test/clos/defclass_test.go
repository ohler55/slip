// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDefclassMinimal(t *testing.T) {
	// There is no undef for a class but a new defclass will replace it or at
	// least the fields in the class.
	(&sliptest.Function{
		Source: `(pretty-print (defclass quux () ()) nil)`,
		Expect: `"(defclass quux ()
  ())
"`,
	}).Test(t)
}
