// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestPartialPanic(t *testing.T) {
	pp := slip.PartialPanic{Message: "partial"}
	tt.Equal(t, "partial", pp.Error())
}
