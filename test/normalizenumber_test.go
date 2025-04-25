// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestNormalizeNumberX(t *testing.T) {
	type data struct {
		v0     slip.Object
		v1     slip.Object
		result string
	}
	for i, d := range []*data{
		{v0: slip.Fixnum(1), v1: slip.Fixnum(2), result: "1 (slip.Fixnum), 2 (slip.Fixnum)"},
	} {
		n0, n1 := slip.NormalizeNumber(d.v0, d.v1)
		tt.Equal(t, d.result, fmt.Sprintf("%s (%T), %s (%T)", n0, n0, n1, n1), "%d: %s %s", i, d.v0, d.v1)
	}
}
