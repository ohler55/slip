// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTimeOk(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let("*trace-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(time (+ 2 3))`,
		Expect: "5",
	}).Test(t)
	tt.Equal(t, "/^Evaluation took:\n  0.[0-9]+ seconds of real time\n$/", out.String())
}
