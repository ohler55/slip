// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net_test

import (
	"fmt"
	"net/http"
	"testing"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestClientGet(t *testing.T) {
	port := 7654

	hs := http.Server{
		Addr: fmt.Sprintf(":%d", port),
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			fmt.Fprintln(w, "Got it!")
		}),
	}
	go hs.ListenAndServe()
	defer hs.Close()

	time.Sleep(time.Second) // TBD remove
	// TBD get free port

	scope := slip.NewScope()
	scope.Let("url", slip.String(fmt.Sprintf("http://localhost:%d", port)))
	tf := &sliptest.Function{
		Scope:  scope,
		Source: `(send (make-instance 'http-client-flavor :url url) :get)`,
		Expect: "/#<response-flavor [0-9a-f]+>/",
	}
	tf.Test(t)
	scope.Let("resp", tf.Result)
	fmt.Printf("*** result: %s\n", tf.Result)

	result := slip.ReadString(`(send resp :status)`).Eval(scope, nil)
	fmt.Printf("*** status: %s\n", result)
}
