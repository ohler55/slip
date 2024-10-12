// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestGetHostByName(t *testing.T) {
	(&sliptest.Function{
		Source: `(get-host-by-name "localhost")`,
		Validate: func(t *testing.T, v slip.Object) {
			inst := v.(*flavors.Instance)
			scope := slip.NewScope()
			tt.Equal(t, slip.String("localhost"), inst.Receive(scope, ":name", nil, 0))
			addrs := inst.Receive(scope, ":addresses", nil, 0).(slip.List)
			for _, addr := range addrs {
				switch len(addr.(slip.Octets)) {
				case 4, 16:
					// ok
				default:
					t.Fail()
				}
			}
		},
	}).Test(t)
}

func TestGetHostByNameNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(get-host-by-name 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
