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
	scope := slip.NewScope()
	for _, name := range slip.ReadEvalString("*all-flavor-names*", scope).(slip.List) {
		if strings.EqualFold("vanilla-flavor", string(name.(slip.Symbol))) {
			return
		}
	}
	t.Errorf("vanilla-flavor not in *all-flavor-names*")
}

func TestFlavorsPackageSetAll(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(setq *all-flavor-names* nil)", scope) })
}

func TestFlavorsVanilla(t *testing.T) {
	scope := slip.NewScope()
	vanilla := slip.ReadEvalString("vanilla-flavor", scope)
	tt.Equal(t, "#<flavor vanilla-flavor>", slip.ObjectString(vanilla))
}
