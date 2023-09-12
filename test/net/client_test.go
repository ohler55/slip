// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net_test

import (
	"fmt"
	"net"
	"net/http"
	"strings"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func availablePort() int {
	addr, err := net.ResolveTCPAddr("tcp", "localhost:0")
	if err != nil {
		panic(err)
	}
	var listener *net.TCPListener
	if listener, err = net.ListenTCP("tcp", addr); err != nil {
		panic(err)
	}
	defer listener.Close()

	return listener.Addr().(*net.TCPAddr).Port
}

func TestClientGet(t *testing.T) {
	port := availablePort()
	hs := http.Server{
		Addr: fmt.Sprintf(":%d", port),
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			fmt.Fprintln(w, "Got it!")
		}),
	}
	go hs.ListenAndServe()
	defer hs.Close()

	su := fmt.Sprintf("http://localhost:%d", port)
	start := time.Now()
	for time.Since(start) < time.Second*2 {
		time.Sleep(time.Millisecond * 50)
		if resp, err := http.Get(su); err == nil {
			resp.Body.Close()
			break
		}
	}
	scope := slip.NewScope()
	scope.Let("url", slip.String(su))
	_ = slip.ReadString(`(defvar client (make-instance 'http-client-flavor :url url))`).Eval(scope, nil)
	tf := &sliptest.Function{
		Scope:  scope,
		Source: `(send client :get)`,
		Expect: "/#<http-response-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	scope.Let("resp", tf.Result)
	result := slip.ReadString(`(send resp :status)`).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(200), result)

	// Instance.Any should already be set.
	tf.Test(t)
	scope.Let("resp", tf.Result)
	result = slip.ReadString(`(send resp :status)`).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(200), result)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-client-flavor :get out)`).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":get"))

	(&sliptest.Function{
		Source:    `(send (make-instance 'http-client-flavor) :get)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send client :get t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)

	(&sliptest.Function{
		Source:    `(send (make-instance 'http-client-flavor :url ":9999") :get)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}
