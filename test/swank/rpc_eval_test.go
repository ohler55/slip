// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"net"
	"strings"
	"testing"
	"time"

	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg/cl" // load CL functions
	"github.com/ohler55/slip/pkg/swank"
)

func TestListenerEval(t *testing.T) {
	scope := slip.NewScope()
	server := swank.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start server: %v", err)
	}
	defer func() { _ = server.Stop() }()

	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	// Create REPL first
	createRepl := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:create-repl"), nil},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	_ = swank.WriteWireMessage(conn, createRepl)
	_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	_, _ = swank.ReadWireMessage(conn, scope)

	// Test listener-eval with simple expression
	evalMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:listener-eval"), slip.String("(+ 2 3)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(2),
	}
	err = swank.WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write eval: %v", err)
	}

	// Read responses until :return
	var gotReturn bool
	for i := 0; i < 10; i++ {
		_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
		response, err := swank.ReadWireMessage(conn, scope)
		if err != nil {
			break
		}
		respList, ok := response.(slip.List)
		if ok && len(respList) > 0 && respList[0] == slip.Symbol(":return") {
			gotReturn = true
			break
		}
	}

	if !gotReturn {
		t.Error("expected :return response")
	}
}

func TestInteractiveEval(t *testing.T) {
	scope := slip.NewScope()
	server := swank.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start server: %v", err)
	}
	defer func() { _ = server.Stop() }()

	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	// Test interactive-eval
	evalMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:interactive-eval"), slip.String("(* 6 7)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := swank.ReadWireMessage(conn, scope)
	if err != nil {
		t.Fatalf("failed to read: %v", err)
	}

	respList, ok := response.(slip.List)
	if !ok || len(respList) < 2 {
		t.Fatalf("invalid response: %v", response)
	}

	if respList[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", respList[0])
	}

	// Check for :ok with result
	inner, ok := respList[1].(slip.List)
	if !ok || len(inner) < 2 {
		t.Fatalf("expected inner list with result")
	}
	if inner[0] != slip.Symbol(":ok") {
		t.Errorf("expected :ok, got %v", inner[0])
	}

	// Result should contain "42"
	resultStr := slip.ObjectString(inner[1])
	if !strings.Contains(resultStr, "42") {
		t.Errorf("expected result containing 42, got %s", resultStr)
	}
}

func TestInteractiveEvalRegion(t *testing.T) {
	scope := slip.NewScope()
	server := swank.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start server: %v", err)
	}
	defer func() { _ = server.Stop() }()

	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	evalMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:interactive-eval-region"), slip.String("(list 1 2 3)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := swank.ReadWireMessage(conn, scope)
	if err != nil {
		t.Fatalf("failed to read: %v", err)
	}

	respList, ok := response.(slip.List)
	if !ok || respList[0] != slip.Symbol(":return") {
		t.Errorf("expected :return response, got %v", response)
	}
}

func TestEvalAndGrabOutput(t *testing.T) {
	scope := slip.NewScope()
	server := swank.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start server: %v", err)
	}
	defer func() { _ = server.Stop() }()

	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	evalMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:eval-and-grab-output"), slip.String("(+ 1 1)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := swank.ReadWireMessage(conn, scope)
	if err != nil {
		t.Fatalf("failed to read: %v", err)
	}

	respList, ok := response.(slip.List)
	if !ok || respList[0] != slip.Symbol(":return") {
		t.Errorf("expected :return response")
	}
}

func TestPprintEval(t *testing.T) {
	scope := slip.NewScope()
	server := swank.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start server: %v", err)
	}
	defer func() { _ = server.Stop() }()

	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	evalMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:pprint-eval"), slip.String("'(a b c)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := swank.ReadWireMessage(conn, scope)
	if err != nil {
		t.Fatalf("failed to read: %v", err)
	}

	respList, ok := response.(slip.List)
	if !ok || respList[0] != slip.Symbol(":return") {
		t.Errorf("expected :return response")
	}
}

func TestEvalWithError(t *testing.T) {
	scope := slip.NewScope()
	server := swank.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start server: %v", err)
	}
	defer func() { _ = server.Stop() }()

	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	// Evaluate expression that will cause an error
	evalMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:interactive-eval"), slip.String("(/ 1 0)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := swank.ReadWireMessage(conn, scope)
	if err != nil {
		t.Fatalf("failed to read: %v", err)
	}

	// Should get :return with :abort
	respList, ok := response.(slip.List)
	if !ok || len(respList) < 2 {
		t.Fatalf("invalid response: %v", response)
	}

	if respList[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", respList[0])
	}
}
