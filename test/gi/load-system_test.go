// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLoadSystemOk(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString("(fmakunbound 'sys-test)", scope).Eval(scope, nil)
	slip.ReadString("(fmakunbound 'sys-test-comp)", scope).Eval(scope, nil)
	slip.ReadString("(fmakunbound 'sister)", scope).Eval(scope, nil)
	slip.ReadString("(fmakunbound 'step-sister)", scope).Eval(scope, nil)
	defer func() {
		slip.ReadString("(fmakunbound 'sys-test)", scope).Eval(scope, nil)
		slip.ReadString("(fmakunbound 'sys-test-comp)", scope).Eval(scope, nil)
		slip.ReadString("(fmakunbound 'sister)", scope).Eval(scope, nil)
		slip.ReadString("(fmakunbound 'step-sister)", scope).Eval(scope, nil)
	}()
	(&sliptest.Function{
		Source: `(let ((*package-load-path* "testdata"))
		           (load-system :sister "testdata/sister") (sister))`,
		Expect: "2",
	}).Test(t)
}

func TestLoadSystemPathList(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString("(fmakunbound 'sys-test)", scope).Eval(scope, nil)
	slip.ReadString("(fmakunbound 'sys-test-comp)", scope).Eval(scope, nil)
	slip.ReadString("(fmakunbound 'sister)", scope).Eval(scope, nil)
	slip.ReadString("(fmakunbound 'step-sister)", scope).Eval(scope, nil)
	defer func() {
		slip.ReadString("(fmakunbound 'sys-test)", scope).Eval(scope, nil)
		slip.ReadString("(fmakunbound 'sys-test-comp)", scope).Eval(scope, nil)
		slip.ReadString("(fmakunbound 'sister)", scope).Eval(scope, nil)
		slip.ReadString("(fmakunbound 'step-sister)", scope).Eval(scope, nil)
	}()
	(&sliptest.Function{
		Source: `(let ((*package-load-path* "testdata"))
		           (load-system :sister '("tyestdata" "testdata/sister")) (sister))`,
		Expect: "2",
	}).Test(t)
}

func TestLoadSystemNoFile(t *testing.T) {
	(&sliptest.Function{
		Source:    `(load-system 'quux "testdata/sister")`,
		PanicType: slip.FileErrorSymbol,
	}).Test(t)
}

func TestLoadSystemAsdExtra(t *testing.T) {
	(&sliptest.Function{
		Source:    `(load-system 'bad-asd "testdata/sister")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestLoadSystemCompNotPlist(t *testing.T) {
	(&sliptest.Function{
		Source:    `(load-system 'bad-plist "testdata/sister")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestLoadSystemCompBadKey(t *testing.T) {
	(&sliptest.Function{
		Source:    `(load-system 'bad-key "testdata/sister")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestLoadSystemBadPathname(t *testing.T) {
	(&sliptest.Function{
		Source:    `(load-system 'bad-path t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestLoadSystemInPackage(t *testing.T) {
	scope := slip.NewScope()
	orig := scope.Get("*package*")
	slip.CurrentPackage.Remove("packer")
	defer func() {
		scope.Set("*package*", orig)
		slip.RemovePackage(slip.FindPackage("packer"))
		slip.CurrentPackage.Remove("outer")
	}()
	(&sliptest.Function{
		Source: `
(load-system :packer "testdata/packer")
(use-package :packer)
(outer)
`,
		Expect: `inside`,
	}).Test(t)
}
