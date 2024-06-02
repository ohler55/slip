// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/sliptest"
)

// TBD test all init vars and gets mostly as an example

func TestSystemFetchFile(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((quux :file "testdata" "sys-test")))))
  (send sys :fetch))
`,
		Expect: `nil`,
	}).Test(t)
	_, err := os.Stat("testout/quux/sys-test.lisp")
	tt.Nil(t, err)
}
