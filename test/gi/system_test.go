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

func TestSystemFetchGitTag(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :scratch ".scratch"
                      :depends-on '((sst :git
                                         "https://github.com/ohler55/slip-system-test"
                                         :tag "v1.0.0"
                                         :sub-dir "lisp")))))
  (send sys :fetch))
`,
		Expect: `nil`,
	}).Test(t)
	_, err := os.Stat("testout/sst/sample.lisp")
	tt.Nil(t, err)
	var content []byte
	content, err = os.ReadFile("testout/sst/sample.lisp")
	tt.Nil(t, err)
	tt.Equal(t, `
(defun sample (x)
  (+ x 5))
`, string(content))
}

// TBD
// https://github.com/ohler55/slip-system-test
// tag v1.0.0
// branch develop
// commit 33e624d04465499b5f19980501bc5fef7e205ba1
