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
	slip.ReadEvalString(`(defflavor f1 ((a 1)) ())`, scope)

	slip.ReadEvalString("(undefflavor 'f1)", scope)

	names := slip.ReadEvalString("*all-flavor-names*", scope)
	tt.Equal(t, false, strings.Contains(names.String(), "f1"))
}

func TestUndefflavorByFlavor(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadEvalString(`(defflavor f1 ((a 1)) ())`, scope)

	slip.ReadEvalString("(undefflavor f1)", scope)

	names := slip.ReadEvalString("*all-flavor-names*", scope)
	tt.Equal(t, false, strings.Contains(names.String(), "f1"))
}

func TestUndefflavorBadFlavor(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(undefflavor t)", scope) })
}

func TestUndefflavorNotdefined(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(undefflavor 'bad)", scope) })
}

func TestUndefflavorBadArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(undefflavor 'bad 'boy)", scope) })
}
