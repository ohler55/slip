// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net_test

import (
	"io"
	"net/http"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/net"
	"github.com/ohler55/slip/sliptest"
)

func sampleResponse() slip.Object {
	return net.MakeResponse(&http.Response{
		StatusCode:    200,
		Proto:         "HTTP/1.1",
		ContentLength: 123,
		Header:        http.Header{"Accept": []string{"text/html", "text/plain"}},
		Trailer:       http.Header{"From": []string{"user@example.ca"}},
		Body:          io.NopCloser(strings.NewReader("Sample body")),
	})
}

func TestResponseStatus(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("response", sampleResponse())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :status)`,
		Expect: "200",
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-response-flavor :status out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":status"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send response :status t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestResponseProtocol(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("response", sampleResponse())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :protocol)`,
		Expect: `"HTTP/1.1"`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-response-flavor :protocol out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":protocol"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send response :protocol t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestResponseContentLength(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("response", sampleResponse())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :content-length)`,
		Expect: "123",
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-response-flavor :content-length out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":content-length"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send response :content-length t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestResponseHeader(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("response", sampleResponse())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :header)`,
		Expect: `(("Accept" "text/html" "text/plain"))`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-response-flavor :header out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":header"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send response :header t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestResponseHeaderGet(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("response", sampleResponse())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :header-get "accept")`,
		Expect: `"text/html"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :header-get "host")`,
		Expect: `nil`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-response-flavor :header-get out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":header-get"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send response :header-get)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send response :header-get 3)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestResponseTrailer(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("response", sampleResponse())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :trailer)`,
		Expect: `(("From" "user@example.ca"))`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-response-flavor :trailer out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":trailer"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send response :trailer t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestResponseTrailerGet(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("response", sampleResponse())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :trailer-get "From")`,
		Expect: `"user@example.ca"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :trailer-get "host")`,
		Expect: `nil`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-response-flavor :trailer-get out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":trailer-get"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send response :trailer-get)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send response :trailer-get 3)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestResponseBody(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("response", sampleResponse())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :body)`,
		Expect: "#<INPUT-STREAM>",
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-response-flavor :body out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":body"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send response :body t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestResponseClose(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("response", sampleResponse())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :close)`,
		Expect: "nil",
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-response-flavor :close out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":close"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send response :close t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}
