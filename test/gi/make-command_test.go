// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMakeCommandOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((cmd (make-command "echo" "one" 2 'three)))
                   (list (send cmd :path)
                         (send cmd :args)))`,
		Expect: `/\(".*echo" \("echo" "one" "2" "three"\)\)/`,
	}).Test(t)
}
