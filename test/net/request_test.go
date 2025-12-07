// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net_test

import (
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
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

	_ = slip.ReadString(`(describe-method http-request-flavor :method out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(describe-method http-request-flavor :url out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(describe-method http-request-flavor :protocol out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(describe-method http-request-flavor :content-length out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(describe-method http-request-flavor :header out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(describe-method http-request-flavor :header-get out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(describe-method http-request-flavor :trailer out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(describe-method http-request-flavor :trailer-get out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(describe-method http-request-flavor :remote-addr out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(describe-method http-request-flavor :body out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(describe-method http-request-flavor :close out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(describe-method http-request-flavor :write out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(send request :write out)`, scope).Eval(scope, nil)
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

	_ = slip.ReadString(`(send request :write t)`, scope).Eval(scope, nil)
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
	tt.Panic(t, func() { _ = slip.ReadString(`(send request :write out)`, scope).Eval(scope, nil) })
}

func TestRequestWriteStdoutError(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("request", sampleRequest())
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString(`(send request :write t)`, scope).Eval(scope, nil) })
}

func TestRequestInit(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-instance 'http-request-flavor
                                 :method "GET"
                                 :protocol "HTTP/1.1"
                                 :url "http://127.0.0.1:9999/test"
                                 :header '(("Accept" "text/plain" "text/html"))
                                 :trailer '(("Transfer-Encoding" "compress"))
                                 :content-length 4
                                 :body "bidy"
                                 :remote-addr "localhost:12345")`,
		Expect: "/#<http-request-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	inst, ok := tf.Result.(*flavors.Instance)
	tt.Equal(t, true, ok)
	var req *http.Request
	req, ok = inst.Any.(*http.Request)
	tt.Equal(t, true, ok)
	tt.Equal(t, "GET", req.Method)
	tt.Equal(t, "localhost:12345", req.RemoteAddr)
	tt.Equal(t, "http://127.0.0.1:9999/test", req.URL.String())
	tt.Equal(t, `{Accept: ["text/plain" "text/html"]}`, pretty.SEN(req.Header))
	tt.Equal(t, `{Transfer-Encoding: [compress]}`, pretty.SEN(req.Trailer))
	tt.Equal(t, "HTTP/1.1", req.Proto)
	tt.Equal(t, "127.0.0.1:9999", req.Host)
	tt.Equal(t, 1, req.ProtoMajor)
	tt.Equal(t, 1, req.ProtoMinor)
	tt.Equal(t, 4, req.ContentLength)
	b, _ := io.ReadAll(req.Body)
	tt.Equal(t, "bidy", string(b))

	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-request-flavor :init out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":init"))
}

func TestRequestInit2(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("body"), slip.NewInputStream(strings.NewReader("bidy")))
	tf := sliptest.Function{
		Scope: scope,
		Source: `(make-instance 'http-request-flavor
                                 :method "GET"
                                 :protocol "HTTP/1.1"
                                 :url "http://127.0.0.1:9999/test"
                                 :content-length 4
                                 :body body)`,
		Expect: "/#<http-request-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	inst, ok := tf.Result.(*flavors.Instance)
	tt.Equal(t, true, ok)
	var req *http.Request
	req, ok = inst.Any.(*http.Request)
	tt.Equal(t, true, ok)
	b, _ := io.ReadAll(req.Body)
	tt.Equal(t, "bidy", string(b))
}

func TestRequestInitBadStrArg(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'http-request-flavor :method t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRequestInitBadHeader(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'http-request-flavor :header t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance 'http-request-flavor :header '(t))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance 'http-request-flavor :header '((t nil)))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance 'http-request-flavor :header '(("Host" t)))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRequestInitBadMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'http-request-flavor :method "BAD")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRequestInitBadProto(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'http-request-flavor :protocol "HTTP/1.x")`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance 'http-request-flavor :protocol "HTTP/x.1")`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestRequestInitBadURL(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'http-request-flavor :url ":#\u0010")`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestRequestInitBadContentLength(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'http-request-flavor :content-length t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRequestInitBadBody(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'http-request-flavor :body t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
