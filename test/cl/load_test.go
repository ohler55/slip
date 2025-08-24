// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"errors"
	"fmt"
	"os"
	"os/user"
	"strings"
	"testing"
	"testing/iotest"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLoadStream(t *testing.T) {
	r := strings.NewReader("(setq load-test-var 7)")
	scope := slip.NewScope()
	scope.Let(slip.Symbol("input"), &slip.InputStream{Reader: r})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(load input)`,
		Expect: "t",
	}).Test(t)
	tt.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("load-test-var")))
}

func TestLoadFile(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(load "testdata/load-me.lisp")`,
		Expect: "t",
	}).Test(t)
	tt.Equal(t, slip.Fixnum(3), scope.Get(slip.Symbol("load-test-me")))
	tt.Equal(t, slip.Fixnum(5), scope.Get(slip.Symbol("load-test-me-too")))
}

func TestLoadRoot(t *testing.T) {
	wd, err := os.Getwd()
	tt.Nil(t, err)
	(&sliptest.Function{
		Source: fmt.Sprintf(`(let ((load-test-me-too 1))
                               (load "%s/testdata/load-me.lisp")
                               load-test-me-too)`, wd),
		Expect: "5",
	}).Test(t)
}

func TestLoadHome(t *testing.T) {
	wd, err := os.Getwd()
	tt.Nil(t, err)
	var usr *user.User
	usr, err = user.Current()
	tt.Nil(t, err)
	if strings.HasPrefix(wd, usr.HomeDir) {
		wd = string(append([]byte{'~'}, strings.TrimPrefix(wd, usr.HomeDir)...))
	}
	(&sliptest.Function{
		Source: fmt.Sprintf(`(let ((load-test-me-too 1))
                               (load "%s/testdata/load-me.lisp")
                               load-test-me-too)`, wd),
		Expect: "5",
	}).Test(t)
}

func TestLoadFileVerbose(t *testing.T) {
	var b strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &b})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(load "testdata/load-me.lisp" :verbose t)`,
		Expect: "t",
	}).Test(t)
	tt.Equal(t, "/;; Loading contents of .*/testdata/load-me.lisp/", b.String())
	tt.Equal(t, "/;; Finished loading .*/testdata/load-me.lisp/", b.String())
}

func TestLoadFileLoadVerbose(t *testing.T) {
	var b strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &b})
	scope.Let(slip.Symbol("*load-verbose*"), slip.True)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(load "testdata/load-me.lisp")`,
		Expect: "t",
	}).Test(t)
	tt.Equal(t, "/;; Loading contents of .*/testdata/load-me.lisp/", b.String())
	tt.Equal(t, "/;; Loading contents of .*/testdata/load-me-too.lisp/", b.String())
	tt.Equal(t, "/;; Finished loading .*/testdata/load-me-too.lisp/", b.String())
	tt.Equal(t, "/;; Finished loading .*/testdata/load-me.lisp/", b.String())
}

func TestLoadFilePrint(t *testing.T) {
	var b strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &b})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(load "testdata/load-me.lisp" :print t)`,
		Expect: "t",
	}).Test(t)
	tt.Equal(t, "/;; Loading contents of .*/testdata/load-me.lisp/", b.String())
	tt.Equal(t, "/;;  3/", b.String())
	tt.Equal(t, "/;;  t/", b.String())
	tt.Equal(t, "/;; Finished loading .*/testdata/load-me.lisp/", b.String())
}

func TestLoadFileLoadPrint(t *testing.T) {
	var b strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &b})
	scope.Let(slip.Symbol("*load-print*"), slip.True)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(load "testdata/load-me.lisp")`,
		Expect: "t",
	}).Test(t)
	tt.Equal(t, "/;; Loading contents of .*/testdata/load-me.lisp/", b.String())
	tt.Equal(t, "/;;  3/", b.String())
	tt.Equal(t, "/;; Loading contents of .*/testdata/load-me-too.lisp/", b.String())
	tt.Equal(t, "/;;  5/", b.String())
	tt.Equal(t, "/;; Finished loading .*/testdata/load-me-too.lisp/", b.String())
	tt.Equal(t, "/;;  t/", b.String())
	tt.Equal(t, "/;; Finished loading .*/testdata/load-me.lisp/", b.String())
}

func TestLoadBadFilespec(t *testing.T) {
	(&sliptest.Function{
		Source: `(load t)`,
		Panics: true,
	}).Test(t)
}

func TestLoadBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(load "testdata/load-me.lisp" 3 4)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(load "testdata/load-me.lisp" :bad 4)`,
		Panics: true,
	}).Test(t)
}

func TestLoadFileNotExist(t *testing.T) {
	(&sliptest.Function{
		Source: `(load "testdata/nothing.lisp" :if-does-not-exist nil)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(load "testdata/nothing.lisp")`,
		Panics: true,
	}).Test(t)
}

func TestLoadReadError(t *testing.T) {
	r := iotest.ErrReader(errors.New("read failed"))
	scope := slip.NewScope()
	scope.Let(slip.Symbol("input"), &slip.InputStream{Reader: r})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(load input)`,
		Panics: true,
	}).Test(t)
}

func TestLoadEmptyPath(t *testing.T) {
	(&sliptest.Function{
		Source:    `(load "")`,
		PanicType: slip.FileErrorSymbol,
	}).Test(t)
}
