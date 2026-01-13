// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk_test

import (
	"net"
	"strings"
	"testing"
	"time"

	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg/cl" // load CL functions
	"github.com/ohler55/slip/pkg/slynk"
	"github.com/ohler55/slip/pkg/swank"
)

func TestListenerEval(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

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

	// Test listener-eval with simple expression
	evalMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:listener-eval"), slip.String("(+ 2 3)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
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
	server := slynk.NewServer(scope)

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
		slip.List{slip.Symbol("slynk:interactive-eval"), slip.String("(* 6 7)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Read responses until :return (may get :write-string first)
	var gotReturn bool
	var resultStr string
	for i := 0; i < 10; i++ {
		_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
		response, err := swank.ReadWireMessage(conn, scope)
		if err != nil {
			break
		}
		respList, ok := response.(slip.List)
		if ok && len(respList) > 0 && respList[0] == slip.Symbol(":return") {
			gotReturn = true
			if len(respList) > 1 {
				if inner, ok := respList[1].(slip.List); ok && len(inner) > 1 {
					resultStr = slip.ObjectString(inner[1])
				}
			}
			break
		}
	}

	if !gotReturn {
		t.Errorf("expected :return response")
	}
	if !strings.Contains(resultStr, "42") {
		t.Errorf("expected result containing 42, got %s", resultStr)
	}
}

func TestInteractiveEvalRegion(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

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
		slip.List{slip.Symbol("slynk:interactive-eval-region"), slip.String("(list 1 2 3)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
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
		t.Errorf("expected :return response")
	}
}

func TestEvalAndGrabOutput(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

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
		slip.List{slip.Symbol("slynk:eval-and-grab-output"), slip.String("(+ 1 1)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
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
		t.Errorf("expected :return response")
	}
}

func TestPPrintEval(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

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
		slip.List{slip.Symbol("slynk:pprint-eval"), slip.String("'(a b c)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
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
		t.Errorf("expected :return response")
	}
}

func TestEvalWithError(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

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
		slip.List{slip.Symbol("slynk:interactive-eval"), slip.String("(/ 1 0)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
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
		t.Errorf("expected :return response")
	}
}
