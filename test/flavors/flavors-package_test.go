// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg"
)

func TestFlavorsPackageAll(t *testing.T) {
	code := slip.ReadString("*all-flavor-names*")
	for _, name := range code.Eval(slip.NewScope()).(slip.List) {
		if strings.EqualFold("vanilla-flavor", string(name.(slip.Symbol))) {
			return
		}
	}
	t.Errorf("vanilla-flavor not in *all-flavor-names*")
}

func TestFlavorsPackageSetAll(t *testing.T) {
	code := slip.ReadString("(setq *all-flavor-names* nil)")
	tt.Panic(t, func() { _ = code.Eval(slip.NewScope()) })
}

func TestFlavorsVanilla(t *testing.T) {
	code := slip.ReadString("vanilla-flavor")

	vanilla := code.Eval(slip.NewScope())
	tt.Equal(t, "#<flavor vanilla-flavor>", slip.ObjectString(vanilla))
}
