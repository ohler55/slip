// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDoSymbolsResultSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst ())) (do-symbols (s *bag* lst) (setq lst (add lst s))))`,
		Validate: func(t *testing.T, v slip.Object) {
			str := slip.ObjectString(v)
			// Check both var and function names.
			for _, name := range []string{
				"*bag*",
				"*bag-time-format*",
				"*bag-time-wrap*",
				"bag-compare",
				"bag-flavor",
				"bag-get",
				"bag-has",
				"bag-modify",
				"bag-native",
				"bag-parse",
				"bag-path-p",
				"bag-read",
				"bag-remove",
				"bag-set",
				"bag-walk",
				"bag-write",
				"json-parse",
				"make-bag",
				"make-bag-path",
			} {
				tt.Equal(t, true, strings.Contains(str, name))
			}
		},
	}).Test(t)
}

func TestDoSymbolsResultForm(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst ())) (do-symbols (s *bag* (join " " lst)) (setq lst (add lst s))))`,
		Validate: func(t *testing.T, v slip.Object) {
			str := string(v.(slip.String))
			// Check both var and function names.
			for _, name := range []string{
				"*bag*",
				"*bag-time-format*",
				"*bag-time-wrap*",
				"bag-compare",
				"bag-flavor",
				"bag-get",
				"bag-has",
				"bag-modify",
				"bag-native",
				"bag-parse",
				"bag-path-p",
				"bag-read",
				"bag-remove",
				"bag-set",
				"bag-walk",
				"bag-write",
				"json-parse",
				"make-bag",
				"make-bag-path",
			} {
				tt.Equal(t, true, strings.Contains(str, name))
			}
		},
	}).Test(t)
}

func TestDoSymbolsJustSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst ())) (do-symbols (s) (setq lst (add lst s))) lst)`,
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
				tt.Equal(t, true, strings.Contains(str, name), "checking %s", name)
			}
		},
	}).Test(t)
}

func TestDoSymbolsReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(do-symbols (s *bag* 3) (when (equal 'bag-get s) (return 7)))`,
		Expect: "7",
	}).Test(t)
	(&sliptest.Function{
		Source: `(block quux (do-symbols (s *bag* 3) (when (equal 'bag-get s) (return-from quux 7))))`,
		Expect: "7",
	}).Test(t)
}

func TestDoSymbolsArgsNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(do-symbols t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDoSymbolsArgsBadSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(do-symbols (t *bag*))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDoSymbolsArgsNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(do-symbols (x t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(do-symbols (x nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
