// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTestPass(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(setq sweet (make-instance 'suite-flavor :name "sweet"))`).Eval(scope, nil)
	sf := sliptest.Function{
		Scope: scope,
		Source: `(make-instance 'test-flavor
                   :name "toot"
                   :parent sweet
                   :forms '((+ 1 2)))`,
		Expect: "/<test-flavor [0-9a-f]+>/",
	}
	sf.Test(t)
	scope.Let(slip.Symbol("toot"), sf.Result)
	_ = slip.ReadString(`(send toot :run)`).Eval(scope, nil)
	result := slip.ReadString(`(send toot :result)`).Eval(scope, nil)
	tt.Equal(t, slip.Symbol(":pass"), result)
	_ = slip.ReadString(`(send toot :report out)`).Eval(scope, nil)
	tt.Equal(t, "/^  toot: .*PASS.*/", out.String())
}

func TestTestFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((/ 1 0))))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run)`).Eval(scope, nil)
	result := slip.ReadString(`(send toot :result)`).Eval(scope, nil)
	tt.Equal(t, slip.Symbol(":fail"), result)
	_ = slip.ReadString(`(send toot :report)`).Eval(scope, nil)
	// tt.Equal(t, "/^toot: .*FAIL.*/", out.String())
}
