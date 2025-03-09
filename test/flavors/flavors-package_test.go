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
	code := slip.ReadString("*all-flavor-names*", scope)
	for _, name := range code.Eval(scope, nil).(slip.List) {
		if strings.EqualFold("vanilla-flavor", string(name.(slip.Symbol))) {
			return
		}
	}
	t.Errorf("vanilla-flavor not in *all-flavor-names*")
}

func TestFlavorsPackageSetAll(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(setq *all-flavor-names* nil)", scope)
	tt.Panic(t, func() { _ = code.Eval(scope, nil) })
}

func TestFlavorsVanilla(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("vanilla-flavor", scope)

	vanilla := code.Eval(scope, nil)
	tt.Equal(t, "#<flavor vanilla-flavor>", slip.ObjectString(vanilla))
}
