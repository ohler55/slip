// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestGetConstant(t *testing.T) {
	vv := slip.CLPkg.GetVarVal("internal-time-units-per-second")
	tt.NotNil(t, vv)
	tt.Equal(t, slip.Fixnum(time.Second), vv.Val)
	tt.Equal(t, "Number of nanoseconds in a second. Internal time units are nanoseconds.", vv.Doc)
}

func TestDefConstAgain(t *testing.T) {
	// No change so it should not panic.
	slip.CurrentPackage.DefConst(
		"internal-time-units-per-second",
		slip.Fixnum(time.Second),
		"Number of nanoseconds in a second. Internal time units are nanoseconds.",
	)
	tt.Panic(t, func() {
		slip.CurrentPackage.DefConst(
			"internal-time-units-per-second",
			slip.Fixnum(300),
			"Number of nanoseconds in a second. Internal time units are nanoseconds.",
		)
	})
}
