// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestSuiteDocs(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)

	_ = slip.ReadString(`(describe-method 'suite-flavor :run)`).Eval(scope, nil)
	tt.Equal(t, "/:run is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :result)`).Eval(scope, nil)
	tt.Equal(t, "/:result is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :report)`).Eval(scope, nil)
	tt.Equal(t, "/:report is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :reset)`).Eval(scope, nil)
	tt.Equal(t, "/:reset is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :init)`).Eval(scope, nil)
	tt.Equal(t, "/:init is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :find)`).Eval(scope, nil)
	tt.Equal(t, "/:find is a method of suite-flavor/", out.String())
}
