// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net_test

import (
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/net"
	"github.com/ohler55/slip/sliptest"
)

type badWriter int

func (w badWriter) Write([]byte) (int, error) {
	return 0, fmt.Errorf("oops")
}

func sampleRequest() slip.Object {
	u, _ := url.Parse("http://localhost:6969")
	return net.MakeRequest(&http.Request{
		Method:        "PUT",
		URL:           u,
		Proto:         "HTTP/1.1",
		ContentLength: 11,
		Header:        http.Header{"Accept": []string{"text/html", "text/plain"}},
		Trailer:       http.Header{"From": []string{"user@example.ca"}},
		Body:          io.NopCloser(strings.NewReader("Sample body")),
		RemoteAddr:    "localhost:12345",
	})
}

func TestRequestMethod(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :method)`,
		Expect: `"PUT"`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :method out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":method"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :method t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestRequestURL(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :url)`,
		Expect: `"http://localhost:6969"`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :url out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":url"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :url t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestRequestProtocol(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :protocol)`,
		Expect: `"HTTP/1.1"`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :protocol out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":protocol"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :protocol t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestRequestContentLength(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :content-length)`,
		Expect: "11",
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :content-length out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":content-length"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :content-length t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestRequestHeader(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :header)`,
		Expect: `(("Accept" "text/html" "text/plain"))`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :header out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":header"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :header t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestRequestHeaderGet(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :header-get "accept")`,
		Expect: `"text/html"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :header-get "host")`,
		Expect: `nil`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :header-get out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":header-get"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :header-get)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :header-get 3)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRequestTrailer(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :trailer)`,
		Expect: `(("From" "user@example.ca"))`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :trailer out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":trailer"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :trailer t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestRequestTrailerGet(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :trailer-get "From")`,
		Expect: `"user@example.ca"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :trailer-get "host")`,
		Expect: `nil`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :trailer-get out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":trailer-get"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :trailer-get)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :trailer-get 3)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRequestRemoteAddr(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :remote-addr)`,
		Expect: `"localhost:12345"`,
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :remote-addr out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":remote-addr"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :remote-addr t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestRequestBody(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :body)`,
		Expect: "#<INPUT-STREAM>",
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :body out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":body"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :body t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestRequestClose(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :close)`,
		Expect: "nil",
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :close out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":close"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :close t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestRequestWriteString(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send request :write nil)`,
		Expect: "\"PUT / HTTP/1.1\r\n" +
			"Host: localhost:6969\r\n" +
			"User-Agent: Go-http-client/1.1\r\n" +
			"Content-Length: 11\r\n" +
			"Accept: text/html\r\n" +
			"Accept: text/plain\r\n" +
			"\r\n" +
			"Sample body\"",
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :write out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":write"))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :write t t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send request :write 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRequestWriteStream(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(send request :write out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, "PUT / HTTP/1.1\r\n"+
		"Host: localhost:6969\r\n"+
		"User-Agent: Go-http-client/1.1\r\n"+
		"Content-Length: 11\r\n"+
		"Accept: text/html\r\n"+
		"Accept: text/plain\r\n"+
		"\r\n"+
		"Sample body", str)
}

func TestRequestWriteStdout(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	var out strings.Builder
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(send request :write t)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, "PUT / HTTP/1.1\r\n"+
		"Host: localhost:6969\r\n"+
		"User-Agent: Go-http-client/1.1\r\n"+
		"Content-Length: 11\r\n"+
		"Accept: text/html\r\n"+
		"Accept: text/plain\r\n"+
		"\r\n"+
		"Sample body", str)
}

func TestRequestWriteStreamError(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString(`(send request :write out)`).Eval(scope, nil) })
}

func TestRequestWriteStdoutError(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString(`(send request :write t)`).Eval(scope, nil) })
}
