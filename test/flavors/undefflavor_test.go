// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestUndefflavorBasic(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defflavor f1 ((a 1)) ())`, scope).Eval(scope, nil)

	slip.ReadString("(undefflavor 'f1)", scope).Eval(scope, nil)

	names := slip.ReadString("*all-flavor-names*", scope).Eval(scope, nil)
	tt.Equal(t, false, strings.Contains(names.String(), "f1"))
}

func TestUndefflavorByFlavor(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defflavor f1 ((a 1)) ())`, scope).Eval(scope, nil)

	slip.ReadString("(undefflavor f1)", scope).Eval(scope, nil)

	names := slip.ReadString("*all-flavor-names*", scope).Eval(scope, nil)
	tt.Equal(t, false, strings.Contains(names.String(), "f1"))
}

func TestUndefflavorBadFlavor(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(undefflavor t)", scope).Eval(scope, nil) })
}

func TestUndefflavorNotdefined(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(undefflavor 'bad)", scope).Eval(scope, nil) })
}

func TestUndefflavorBadArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(undefflavor 'bad 'boy)", scope).Eval(scope, nil) })
}
