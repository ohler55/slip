// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestGetConstant(t *testing.T) {
	value, doc, has := slip.GetConstant(slip.Symbol("internal-time-units-per-second"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Fixnum(time.Second), value)
	tt.Equal(t, "Number of nanoseconds in a second. Internal time units are nanoseconds.", doc)
}

func TestDefConstantAgain(t *testing.T) {
	// No change so it should not panic.
	slip.DefConstant(
		slip.Symbol("internal-time-units-per-second"),
		slip.Fixnum(time.Second),
		"Number of nanoseconds in a second. Internal time units are nanoseconds.",
	)
	tt.Panic(t, func() { slip.DefConstant(slip.Symbol("internal-time-units-per-second"), slip.Fixnum(300), "") })
}
