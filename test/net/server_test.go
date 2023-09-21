// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net_test

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestServerBasic(t *testing.T) {
	port := availablePort()
	scope := slip.NewScope()
	server := slip.ReadString(
		fmt.Sprintf(`(make-instance 'http-server-flavor :address ":%d")`, port),
	).Eval(scope, nil)
	scope.Let("server", server)
	defer func() { _ = slip.ReadString(`(send server :shutdown)`).Eval(scope, nil) }()
	_ = slip.ReadString(`(send server :add-handler
                                         "/test"
                                         (lambda (w r) (send w :write "Thank you.")))`).Eval(scope, nil)
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
}
