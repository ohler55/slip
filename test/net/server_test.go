// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net_test

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"strings"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestServerBasic(t *testing.T) {
	port := availablePort()
	scope := slip.NewScope()
	server := slip.ReadString(
		fmt.Sprintf(`(make-instance 'http-server-flavor
                                     :maximum-header-length 1024
                                     :read-timeout 3.0
                                     :write-timeout 4.0
                                     :idle-timeout 5.0
                                     :address ":%d")`, port),
		scope).Eval(scope, nil)
	scope.Let("server", server)
	defer func() { _ = slip.ReadString(`(send server :shutdown)`, scope).Eval(scope, nil) }()
	_ = slip.ReadString(`(send server :add-handler
                                         "/test"
                                         (lambda (w r) (send w :write "Thank you.")))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send server :add-handler
                                         "/sample/"
                                         "testdata")`, scope).Eval(scope, nil)
	sinst := server.(*flavors.Instance)
	hs := sinst.Any.(*http.Server)
	tt.Equal(t, time.Second*3, hs.ReadTimeout)
	tt.Equal(t, time.Second*4, hs.WriteTimeout)
	tt.Equal(t, time.Second*5, hs.IdleTimeout)
	tt.Equal(t, 1024, hs.MaxHeaderBytes)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send server :start 1.0)`,
		Expect: "nil",
	}).Test(t)
	cx, cf := context.WithTimeout(context.Background(), time.Second)
	defer cf()
	req, err := http.NewRequestWithContext(cx, "GET", fmt.Sprintf("http://localhost:%d/test", port), nil)
	tt.Nil(t, err)
	var res *http.Response
	res, err = (&http.Client{}).Do(req)
	tt.Nil(t, err)
	defer func() { _ = res.Body.Close() }()
	body, _ := io.ReadAll(res.Body)
	tt.Equal(t, 200, res.StatusCode)
	tt.Equal(t, "Thank you.", string(body))
	_ = res.Body.Close()

	req, err = http.NewRequestWithContext(cx, "GET", fmt.Sprintf("http://localhost:%d/sample/sample.txt", port), nil)
	tt.Nil(t, err)
	res, err = (&http.Client{}).Do(req)
	tt.Nil(t, err)
	body, _ = io.ReadAll(res.Body)
	tt.Equal(t, 200, res.StatusCode)
	tt.Equal(t, "A sample file.\n", string(body))
	_ = slip.ReadString(`(send server :shutdown t)`, scope).Eval(scope, nil)
}

func TestServerInitFail(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'http-server-flavor :idle-timeout t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance 'http-server-flavor :maximum-header-length t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestServerDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method http-server-flavor :init out)`, scope).Eval(scope, nil)
	str := out.String()
	tt.Equal(t, true, strings.Contains(str, ":init"))

	out.Reset()
	_ = slip.ReadString(`(describe-method http-server-flavor :start out)`, scope).Eval(scope, nil)
	str = out.String()
	tt.Equal(t, true, strings.Contains(str, ":start"))

	out.Reset()
	_ = slip.ReadString(`(describe-method http-server-flavor :shutdown out)`, scope).Eval(scope, nil)
	str = out.String()
	tt.Equal(t, true, strings.Contains(str, ":shutdown"))

	out.Reset()
	_ = slip.ReadString(`(describe-method http-server-flavor :add-handler out)`, scope).Eval(scope, nil)
	str = out.String()
	tt.Equal(t, true, strings.Contains(str, ":add-handler"))
}

func TestServerMethodArgFail(t *testing.T) {
	scope := slip.NewScope()
	server := slip.ReadString(`(make-instance 'http-server-flavor)`, scope).Eval(scope, nil)
	scope.Let("server", server)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send server :start t t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send server :start t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send server :start 0.1)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send server :shutdown t t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send server :add-handler "/test")`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send server :add-handler t nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
