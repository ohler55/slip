// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net_test

import (
	"net/http"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/net"
	"github.com/ohler55/slip/sliptest"
)

func TestResponse(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("response", net.MakeResponse(&http.Response{StatusCode: 200}))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send response :status)`,
		Expect: "200",
	}).Test(t)
}
