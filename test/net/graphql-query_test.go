// Copyright (c) 2025, Peter Ohler, All rights reserved.

package net_test

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestGraphQLQueryOk(t *testing.T) {
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

	var recvLog bytes.Buffer
	scope.Let("recv-log", &slip.OutputStream{Writer: &recvLog})
	defer func() { _ = slip.ReadString(`(send server :shutdown)`, scope).Eval(scope, nil) }()
	_ = slip.ReadString(
		`(send server :add-handler "/graphql"
                                   (lambda (w r)
                                     (format recv-log "~A: ~A~%~A~%"
                                             (send r :method)
                                             (send r :header)
                                             (read-all (send r :body)))
                                     (send w :write "{\"data\":{\"user\":{\"name\":\"Fred\"}}}")))`,
		scope).Eval(scope, nil)

	_ = slip.ReadString(`(send server :start 1.0)`, scope).Eval(scope, nil)

	(&sliptest.Function{
		Scope: scope,
		Source: fmt.Sprintf(`(bag-write
                              (graphql-query
                               "http://localhost:%d/graphql"
                               "user(id:~S group:\"~=group~=\") {name}"
                               :template-args '("id-123")
                               :group "one")
                             :pretty t)`, port),
		Expect: `"{data: {user: {name: Fred}}}"`,
	}).Test(t)

	tt.Equal(t, `/POST: /`, recvLog.String())
	tt.Equal(t, `/"Content-Type" "application/graphql"/`, recvLog.String())
	tt.Equal(t, `/user\(id:"id-123" group:"one"\) \{name\}/`, recvLog.String())

	recvLog.Reset()
	(&sliptest.Function{
		Scope: scope,
		Source: fmt.Sprintf(`(bag-write
                              (graphql-query
                               "http://localhost:%d/graphql"
                               "user(id:~=id~= group:\"~=group~=\") {name}"
                               '(("Content-Type" "application/json"))
                               :id "1d-123"
                               :group "one")
                             :pretty t)`, port),
		Expect: `"{data: {user: {name: Fred}}}"`,
	}).Test(t)

	tt.Equal(t, `/"Content-Type" "application/json"/`, recvLog.String())
	// a JSON request
	tt.Equal(t, `/\{"query":"user\(id:1d-123 group:\\"one\\"\) \{name\}"}/`, recvLog.String())

	(&sliptest.Function{
		Scope: scope,
		Source: fmt.Sprintf(`(bag-write
                              (graphql-query
                               "http://localhost:%d/graphql"
                               "user(id:~S group:\"~=group~=\") {name}"
                               nil
                               :template-args '("id-123")
                               :group "one")
                             :pretty t)`, port),
		Expect: `"{data: {user: {name: Fred}}}"`,
	}).Test(t)
}

func TestGraphQLQueryNoServer(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`(graphql-query
                              "http://localhost:%d/graphql"
                              "user(id:~S group:~S) {name}"
                              :timeout 0.01
                              :template-args '("id-123" "one"))`, port),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestGraphQLQueryBadTemplateArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(graphql-query
                  "http://localhost:7777/graphql"
                  "user(id:\"id-123\" group:\"one\") {name}"
                  :template-args "id-123")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestGraphQLQueryBadTimeout(t *testing.T) {
	(&sliptest.Function{
		Source: `(graphql-query
                  "http://localhost:7777/graphql"
                  "user(id:\"id-123\" group:\"one\") {name}"
                  :timeout t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestGraphQLQueryBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(graphql-query
                  "http://localhost:7777/graphql"
                  "user(id:\"id-123\" group:\"one\") {name}"
                  t t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestGraphQLQueryRequestError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(graphql-query ":::::::::" "user(id:\"id-123\" group:\"one\") {name}")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
