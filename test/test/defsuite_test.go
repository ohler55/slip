// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestDefsuiteBasic(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq top
                               (defsuite "top" nil
                                         :setup (lambda () (setq foo 3))
                                         :teardown (lambda () (setq foo 4))))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send top :describe out)`, scope).Eval(scope, nil)
	tt.Equal(t, `/#<suite-flavor [0-9a-f]+>, an instance of flavor suite-flavor,
  has instance variable values:
    children: nil
    name: "top"
    parent: nil
    setup: #<function \(lambda \(\)\) \{[0-9a-f]+\}>
    teardown: #<function \(lambda \(\)\) \{[0-9a-f]+\}>
/`, out.String())

	out.Reset()
	_ = slip.ReadString(`(setq sweet (defsuite "sweet" top))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send top :describe out)`, scope).Eval(scope, nil)
	tt.Equal(t, `/#<suite-flavor [0-9a-f]+>, an instance of flavor suite-flavor,
  has instance variable values:
    children: \(#<suite-flavor [0-9a-f]+>\)
    name: "top"
    parent: nil
    setup: #<function \(lambda \(\)\) \{[0-9a-f]+\}>
    teardown: #<function \(lambda \(\)\) \{[0-9a-f]+\}>
/`, out.String())
}

func TestDefsuiteBadName(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString(`(defsuite t nil)`, scope).Eval(scope, nil) })
}

func TestDefsuiteBadParent(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString(`(defsuite "bad" t)`, scope).Eval(scope, nil) })
}
