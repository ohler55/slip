// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"path/filepath"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func validateTruename(t *testing.T, v slip.Object) {
	t.Helper()

	s, ok := v.(slip.String)
	tt.Equal(t, true, ok, "expected truename to return a string, got %T", v)

	path := string(s)
	tt.Equal(t, true, filepath.IsAbs(path), "expected absolute path, got %q", path)
	tt.Equal(t, true, strings.HasSuffix(filepath.ToSlash(path), "/test/cl/truename_test.go"),
		"expected path to end with /test/cl/truename_test.go, got %q", path)
}

func TestTruenameStringOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(truename "truename_test.go")`,
		Validate: validateTruename,
	}).Test(t)
}

func TestTruenameStringNoFile(t *testing.T) {
	(&sliptest.Function{
		Source:    `(truename "truename_test.lisp")`,
		PanicType: slip.FileErrorSymbol,
	}).Test(t)
}

func TestTruenameNotFilespec(t *testing.T) {
	(&sliptest.Function{
		Source:    `(truename t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestTruenameFileStream(t *testing.T) {
	(&sliptest.Function{
		Source:   `(with-open-file (f "truename_test.go" :direction :input) (truename f))`,
		Validate: validateTruename,
	}).Test(t)
}

func TestTruenameSynonymStreamOk(t *testing.T) {
	slip.CurrentPackage.Set("zz", nil)
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()

	(&sliptest.Function{
		Source: `(with-open-file (f "truename_test.go" :direction :input)
                  (let ((ss (make-synonym-stream 'zz)))
                   (setq zz f)
                   (truename ss)))`,
		Validate: validateTruename,
	}).Test(t)
}

func TestTruenameSynonymStreamUnbound(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((ss (make-synonym-stream 'zz)))
                  (truename ss))`,
		PanicType: slip.UnboundVariableSymbol,
	}).Test(t)
}
