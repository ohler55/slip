// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/pkg/wos"
	"github.com/ohler55/slip/sliptest"
)

func TestFlavor(t *testing.T) {
	tt.Equal(t, "wos-flavor", wos.Flavor().Name())
}

func TestMakeWos(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-wos)`,
		Expect: "/#<wos-flavor [0-9a-f]+>/",
	}).Test(t)
}

func TestMakeInstanceWos(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-instance 'wos-flavor)`,
		Expect: "/#<wos-flavor [0-9a-f]+>/",
	}).Test(t)
}
