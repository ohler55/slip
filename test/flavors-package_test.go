// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

func TestFlavorsPackageAll(t *testing.T) {
	code := slip.ReadString("*all-flavor-names*")
	for _, name := range code.Eval(slip.NewScope()).(slip.List) {
		if strings.EqualFold("vanilla-flavor", string(name.(slip.Symbol))) {
			return
		}
	}
	require.Fail(t, "vanilla-flavor not in *all-flavor-names*")
}

func TestFlavorsPackageSetAll(t *testing.T) {
	code := slip.ReadString("(setq *all-flavor-names* nil)")
	require.Panics(t, func() { _ = code.Eval(slip.NewScope()) })
}

func TestFlavorsVanilla(t *testing.T) {
	code := slip.ReadString("vanilla-flavor")

	vanilla := code.Eval(slip.NewScope())
	require.Equal(t, "#<flavor vanilla-flavor>", slip.ObjectString(vanilla))
}
