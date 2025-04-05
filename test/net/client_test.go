// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net_test

import (
	"fmt"
	"io"
	"net/http"
	"strings"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestClientGet(t *testing.T) {
	port := availablePort()
	hs := http.Server{
		Addr: fmt.Sprintf(":%d", port),
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			_, _ = fmt.Fprintln(w, "Got it!")
		}),
	}
	go func() { _ = hs.ListenAndServe() }()
	defer func() { _ = hs.Close() }()

	su := fmt.Sprintf("http://localhost:%d", port)
	start := time.Now()
	for time.Since(start) < time.Second*2 {
		time.Sleep(time.Millisecond * 50)
		if resp, err := http.Get(su); err == nil {
			_ = resp.Body.Close()
			break
		}
	}
	scope := slip.NewScope()
	scope.Let("url", slip.String(su))
	_ = slip.ReadString(`(setq client (make-instance 'http-client-flavor :url url))`, scope).Eval(scope, nil)
	tf := &sliptest.Function{
		Scope:  scope,
		Source: `(send client :get)`,
		Expect: "/#<http-response-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	scope.Let("resp", tf.Result)
	_ = slip.ReadString(`(send resp :close)`, scope).Eval(scope, nil)
	result := slip.ReadString(`(send resp :status)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(200), result)

	// Instance.Any should already be set.
	tf.Test(t)
	scope.Let("resp", tf.Result)
	_ = slip.ReadString(`(send resp :close)`, scope).Eval(scope, nil)
	result = slip.ReadString(`(send resp :status)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(200), result)

	tf = &sliptest.Function{
		Scope:  scope,
		Source: `(send client :get '(("Accept" "text/plain")))`,
		Expect: "/#<http-response-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	scope.Let("resp", tf.Result)
	_ = slip.ReadString(`(send resp :close)`, scope).Eval(scope, nil)
	result = slip.ReadString(`(send resp :status)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(200), result)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-client-flavor :get out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":get"))

	(&sliptest.Function{
		Source:    `(send (make-instance 'http-client-flavor) :get)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send client :get t t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send client :get t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send client :get '(t))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send client :get '((t "text/plain")))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send client :get '(("Accept" t)))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)

	(&sliptest.Function{
		Source:    `(send (make-instance 'http-client-flavor :url ":#\u0010") :get '(("Accept" "text/plain")))`,
		PanicType: slip.Symbol("error"),
	}).Test(t)

	(&sliptest.Function{
		Source:    `(send (make-instance 'http-client-flavor :url "http://localhost:9999") :get)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestClientPutString(t *testing.T) {
	port := availablePort()
	hs := http.Server{
		Addr: fmt.Sprintf(":%d", port),
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if r.Body == nil { // the GET ping
				_, _ = w.Write([]byte("ready"))
				return
			}
			b, err := io.ReadAll(r.Body)
			if err != nil {
				_, _ = fmt.Fprintf(w, "error: %s\n", err)
			} else {
				_, _ = w.Write([]byte(r.Header.Get("Content-Type")))
				_, _ = w.Write([]byte{'-'})
				_, _ = w.Write(b)
			}
		}),
	}
	go func() { _ = hs.ListenAndServe() }()
	defer func() { _ = hs.Close() }()

	su := fmt.Sprintf("http://localhost:%d", port)
	start := time.Now()
	for time.Since(start) < time.Second*2 {
		time.Sleep(time.Millisecond * 50)
		if resp, err := http.Get(su); err == nil {
			_ = resp.Body.Close()
			break
		}
	}
	scope := slip.NewScope()
	scope.Let("url", slip.String(su))
	_ = slip.ReadString(`(setq client (make-instance 'http-client-flavor :url url))`, scope).Eval(scope, nil)
	tf := &sliptest.Function{
		Scope:  scope,
		Source: `(send client :put "Putty" "text/plain")`,
		Expect: "/#<http-response-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	scope.Let("resp", tf.Result)
	result := slip.ReadString(`(send resp :status)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(200), result)
	result = slip.ReadString(`(read-all (send resp :body))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send resp :close)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.String("text/plain-Putty"), result)

	tf = &sliptest.Function{
		Scope:  scope,
		Source: `(send client :put "Putty" "text/plain" '(("Content-type" "text/html")))`,
		Expect: "/#<http-response-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	scope.Let("resp", tf.Result)
	result = slip.ReadString(`(send resp :status)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(200), result)
	result = slip.ReadString(`(read-all (send resp :body))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send resp :close)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.String("text/html-Putty"), result)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send client :put "Putty" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send client :put "Putty" t '(("Content-type" "text/html")))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send client :put "Putty" "text/plain" '(("Content-type" "text/html")) t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-instance 'http-client-flavor :url ":#\u0010") :put "x")`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-instance 'http-client-flavor :url "http://localhost:9999") :put "x")`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send client :put (make-instance 'vanilla-flavor))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send client :put t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-client-flavor :put out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":put"))
}

func TestClientPostBag(t *testing.T) {
	port := availablePort()
	hs := http.Server{
		Addr: fmt.Sprintf(":%d", port),
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if r.Body == nil { // the GET ping
				_, _ = w.Write([]byte("ready"))
				return
			}
			b, err := io.ReadAll(r.Body)
			if err != nil {
				_, _ = fmt.Fprintf(w, "error: %s\n", err)
			} else {
				_, _ = w.Write([]byte(r.Header.Get("Content-Type")))
				_, _ = w.Write([]byte{'-'})
				_, _ = w.Write(b)
			}
		}),
	}
	go func() { _ = hs.ListenAndServe() }()
	defer func() { _ = hs.Close() }()

	su := fmt.Sprintf("http://localhost:%d", port)
	start := time.Now()
	for time.Since(start) < time.Second*2 {
		time.Sleep(time.Millisecond * 50)
		if resp, err := http.Get(su); err == nil {
			_ = resp.Body.Close()
			break
		}
	}
	scope := slip.NewScope()
	scope.Let("url", slip.String(su))
	_ = slip.ReadString(`(setq client (make-instance 'http-client-flavor :url url))`, scope).Eval(scope, nil)
	tf := &sliptest.Function{
		Scope:  scope,
		Source: `(send client :post (make-instance 'bag-flavor :parse "{x:3}"))`,
		Expect: "/#<http-response-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	scope.Let("resp", tf.Result)
	result := slip.ReadString(`(send resp :status)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(200), result)
	result = slip.ReadString(`(read-all (send resp :body))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send resp :close)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.String(`application/json-{"x":3}`), result)

	tf = &sliptest.Function{
		Scope:  scope,
		Source: `(send client :post (make-instance 'bag-flavor :parse "{x:3}") nil)`,
		Expect: "/#<http-response-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	scope.Let("resp", tf.Result)
	result = slip.ReadString(`(send resp :status)`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send resp :close)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(200), result)

	tf = &sliptest.Function{
		Scope:  scope,
		Source: `(send client :post (make-instance 'bag-flavor :parse "{x:3}") nil '())`,
		Expect: "/#<http-response-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	scope.Let("resp", tf.Result)
	result = slip.ReadString(`(send resp :status)`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send resp :close)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(200), result)
}

func TestClientPutStream(t *testing.T) {
	port := availablePort()
	hs := http.Server{
		Addr: fmt.Sprintf(":%d", port),
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if r.Body == nil { // the GET ping
				_, _ = w.Write([]byte("ready"))
				return
			}
			b, err := io.ReadAll(r.Body)
			if err != nil {
				_, _ = fmt.Fprintf(w, "error: %s\n", err)
			} else {
				_, _ = w.Write([]byte(r.Header.Get("Content-Type")))
				_, _ = w.Write([]byte{'-'})
				_, _ = w.Write(b)
			}
		}),
	}
	go func() { _ = hs.ListenAndServe() }()
	defer func() { _ = hs.Close() }()

	su := fmt.Sprintf("http://localhost:%d", port)
	start := time.Now()
	for time.Since(start) < time.Second*2 {
		time.Sleep(time.Millisecond * 50)
		if resp, err := http.Get(su); err == nil {
			_ = resp.Body.Close()
			break
		}
	}
	scope := slip.NewScope()
	scope.Let("url", slip.String(su))
	_ = slip.ReadString(`(setq client (make-instance 'http-client-flavor :url url))`, scope).Eval(scope, nil)
	tf := &sliptest.Function{
		Scope:  scope,
		Source: `(send client :put (make-string-input-stream "Posty") nil '(("Content-Type" "text/plain")))`,
		Expect: "/#<http-response-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	scope.Let("resp", tf.Result)
	result := slip.ReadString(`(send resp :status)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(200), result)
	result = slip.ReadString(`(read-all (send resp :body))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send resp :close)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.String(`text/plain-Posty`), result)
}
