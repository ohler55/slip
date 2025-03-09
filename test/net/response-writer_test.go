// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net_test

import (
	"fmt"
	"net/http"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

type respWriter struct {
	header  http.Header
	content []byte
	code    int
	fail    bool
}

func (rw *respWriter) Header() http.Header {
	return rw.header
}

func (rw *respWriter) Write(b []byte) (int, error) {
	if rw.fail {
		return 0, fmt.Errorf("failed")
	}
	rw.content = append(rw.content, b...)
	return len(b), nil
}

func (rw *respWriter) WriteHeader(code int) {
	rw.code = code
}

func TestResponseWriterMethods(t *testing.T) {
	rw := respWriter{header: http.Header{"Content-Length": []string{"123"}}}
	scope := slip.NewScope()
	rwf := flavors.Find("http-response-writer-flavor")
	rwi := rwf.MakeInstance().(*flavors.Instance)
	rwi.Any = &rw
	scope.Let("rw", rwi)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send rw :header)`,
		Expect: `(("Content-Length" "123"))`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send rw :header-get "Content-Length")`,
		Expect: `"123"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send rw :header-get "Nothing")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send rw :write-status 201)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, 201, rw.code)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send rw :write "This is the contents.")`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "This is the contents.", string(rw.content))
}

func TestResponseWriterDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-response-writer-flavor :header out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":header"))

	out.Reset()
	_ = slip.ReadString(`(describe-method http-response-writer-flavor :header-get out)`, scope).Eval(scope, nil)
	str = out.String()
	tt.Equal(t, true, strings.Contains(str, ":header-get"))

	out.Reset()
	_ = slip.ReadString(`(describe-method http-response-writer-flavor :write-status out)`, scope).Eval(scope, nil)
	str = out.String()
	tt.Equal(t, true, strings.Contains(str, ":write-status"))

	out.Reset()
	_ = slip.ReadString(`(describe-method http-response-writer-flavor :write out)`, scope).Eval(scope, nil)
	str = out.String()
	tt.Equal(t, true, strings.Contains(str, ":write"))
}

func TestResponseWriterMethodPanics(t *testing.T) {
	rw := respWriter{header: http.Header{"Content-Length": []string{"123"}}}
	scope := slip.NewScope()
	rwf := flavors.Find("http-response-writer-flavor")
	rwi := rwf.MakeInstance().(*flavors.Instance)
	rwi.Any = &rw
	scope.Let("rw", rwi)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send rw :header t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send rw :header-get)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send rw :header-get t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send rw :write-status)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send rw :write-status t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send rw :write)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send rw :write t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	rw.fail = true
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send rw :write "bad")`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}
