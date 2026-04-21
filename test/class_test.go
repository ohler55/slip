// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestFindClass(t *testing.T) {
	c := slip.FindClass("vanilla-flavor")
	tt.NotNil(t, c)
	tt.Equal(t, "vanilla-flavor", c.Name())

	c = slip.FindClass("flavors:vanilla-flavor")
	tt.NotNil(t, c)
	tt.Equal(t, "vanilla-flavor", c.Name())

	c = slip.FindClass("quux:vanilla-flavor")
	tt.Nil(t, c)
}
