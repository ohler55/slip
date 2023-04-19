// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestUndefflavorBasic(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defflavor f1 ((a 1)) ())`).Eval(scope, nil)

	slip.ReadString("(undefflavor 'f1)").Eval(scope, nil)

	names := slip.ReadString("*all-flavor-names*").Eval(scope, nil)
	tt.Equal(t, "(bag-flavor logger vanilla-flavor)", names.String())
}

func TestUndefflavorByFlavor(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defflavor f1 ((a 1)) ())`).Eval(scope, nil)

	slip.ReadString("(undefflavor f1)").Eval(scope, nil)

	names := slip.ReadString("*all-flavor-names*").Eval(scope, nil)
	tt.Equal(t, "(bag-flavor logger vanilla-flavor)", names.String())
}

func TestUndefflavorBadFlavor(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString("(undefflavor t)").Eval(slip.NewScope(), nil) })
}

func TestUndefflavorNotdefined(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString("(undefflavor 'bad)").Eval(slip.NewScope(), nil) })
}

func TestUndefflavorBadArgCount(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString("(undefflavor 'bad 'boy)").Eval(slip.NewScope(), nil) })
}
