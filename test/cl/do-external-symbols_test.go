// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDoExternalSymbolsResultSymbol(t *testing.T) {
	scope := slip.NewScope()
	defer func() {
		slip.RemovePackage(slip.FindPackage("do-x-sym-pack-1"))
	}()
	_ = slip.ReadString("(defvar do-x-sym-pack-1 (defpackage 'do-x-sym-pack-1 (:export v1 v2)))").Eval(scope, nil)
	_ = slip.ReadString("(defvar do-x-sym-pack-1::v2 2)").Eval(scope, nil) // exported
	_ = slip.ReadString("(defvar do-x-sym-pack-1::v3 3)").Eval(scope, nil) // private

	(&sliptest.Function{
		Source: `(let ((lst ())) (do-external-symbols (s do-x-sym-pack-1 lst) (setq lst (add lst s))))`,
		Validate: func(t *testing.T, v slip.Object) {
			str := slip.ObjectString(v)
			// Check both var and function names.
			for _, name := range []string{
				"v1",
				"v2",
			} {
				tt.Equal(t, true, strings.Contains(str, name))
			}
		},
	}).Test(t)
}

func TestDoExternalSymbolsResultForm(t *testing.T) {
	scope := slip.NewScope()
	defer func() {
		slip.RemovePackage(slip.FindPackage("do-x-sym-pack-2"))
	}()
	_ = slip.ReadString("(defvar do-x-sym-pack-2 (defpackage 'do-x-sym-pack-2 (:export v1 v2)))").Eval(scope, nil)
	_ = slip.ReadString("(defvar do-x-sym-pack-2::v2 2)").Eval(scope, nil) // exported
	_ = slip.ReadString("(defvar do-x-sym-pack-2::v3 3)").Eval(scope, nil) // private

	(&sliptest.Function{
		Source: `(let ((lst ())) (do-external-symbols (s do-x-sym-pack-2 (join " " lst)) (setq lst (add lst s))))`,
		Validate: func(t *testing.T, v slip.Object) {
			str := string(v.(slip.String))
			// Check both var and function names.
			for _, name := range []string{
				"v1",
				"v2",
			} {
				tt.Equal(t, true, strings.Contains(str, name))
			}
		},
	}).Test(t)
}

func TestDoExternalSymbolsJustSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst ())) (do-external-symbols (s) (setq lst (add lst s))) lst)`,
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

func TestDoExternalSymbolsReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(do-external-symbols (s *bag* 3) (when (equal 'bag-get s) (return 7)))`,
		Expect: "7",
	}).Test(t)
	(&sliptest.Function{
		Source: `(block quux (do-external-symbols (s *bag* 3) (when (equal 'bag-get s) (return-from quux 7))))`,
		Expect: "7",
	}).Test(t)
}

func TestDoExternalSymbolsArgsNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(do-external-symbols t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDoExternalSymbolsArgsBadSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(do-external-symbols (t *bag*))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDoExternalSymbolsArgsNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(do-external-symbols (x t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(do-external-symbols (x nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
