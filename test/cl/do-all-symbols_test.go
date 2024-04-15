// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDoAllSymbolsResultSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst ())) (do-all-symbols (s lst) (setq lst (add lst s))))`,
		Validate: func(t *testing.T, v slip.Object) {
			str := slip.ObjectString(v)
			// Check a var and function from multiple packages.
			for _, name := range []string{
				"*bag*",
				"bag-compare",
				"*clos*",
				"make-instance",
				"*flavors*",
				"vanilla-flavor",
				"*print-pretty*",
			} {
				tt.Equal(t, true, strings.Contains(str, name))
			}
		},
	}).Test(t)
}

func TestDoAllSymbolsResultForm(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst ())) (do-all-symbols (s (join " " lst)) (setq lst (add lst s))))`,
		Validate: func(t *testing.T, v slip.Object) {
			str := string(v.(slip.String))
			// Check a var and function from multiple packages.
			for _, name := range []string{
				"*bag*",
				"bag-compare",
				"*clos*",
				"make-instance",
				"*flavors*",
				"vanilla-flavor",
				"*print-pretty*",
			} {
				tt.Equal(t, true, strings.Contains(str, name))
			}
		},
	}).Test(t)
}

func TestDoAllSymbolsJustSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst ())) (do-all-symbols (s) (setq lst (add lst s))) lst)`,
		Validate: func(t *testing.T, v slip.Object) {
			str := slip.ObjectString(v)
			// Check a var and function from multiple packages.
			for _, name := range []string{
				"*bag*",
				"bag-compare",
				"*clos*",
				"make-instance",
				"*flavors*",
				"vanilla-flavor",
				"*print-pretty*",
			} {
				tt.Equal(t, true, strings.Contains(str, name))
			}
		},
	}).Test(t)
}

func TestDoAllSymbolsReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(do-all-symbols (s 3) (when (equal 'bag-get s) (return 7)))`,
		Expect: "7",
	}).Test(t)
	(&sliptest.Function{
		Source: `(block quux (do-all-symbols (s 3) (when (equal 'bag-get s) (return-from quux 7))))`,
		Expect: "7",
	}).Test(t)
}

func TestDoAllSymbolsArgsNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(do-all-symbols t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDoAllSymbolsArgsBadSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(do-all-symbols (t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
