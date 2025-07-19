// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

func TestSimpleFormatter(t *testing.T) {
	var sf cl.SimpleFormatterEmbed

	sf.Init(slip.NewScope(), "- ~A -", slip.List{slip.Fixnum(7)})
	tt.Equal(t, "- ~A -", sf.Control())
	tt.Equal(t, slip.List{slip.Fixnum(7)}, sf.Arguments())
	tt.Equal(t, "- 7 -", sf.Output())
}
