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

func TestSlynkMacroexpand1(t *testing.T) {
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

	// First create an MREPL channel
	createMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:create-mrepl"), nil},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	_ = swank.WriteWireMessage(conn, createMsg)
	for range 5 {
		_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
		resp, err := swank.ReadWireMessage(conn, scope)
		if err != nil {
			break
		}
		if list, ok := resp.(slip.List); ok && len(list) > 0 && list[0] == slip.Symbol(":return") {
			break
		}
	}

	// Define a macro via listener-eval
	evalMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:listener-eval"), slip.String("(defmacro double (x) `(* ,x 2))")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(2),
	}
	_ = swank.WriteWireMessage(conn, evalMsg)
	for range 5 {
		_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
		resp, err := swank.ReadWireMessage(conn, scope)
		if err != nil {
			break
		}
		if list, ok := resp.(slip.List); ok && len(list) > 0 && list[0] == slip.Symbol(":return") {
			break
		}
	}

	// Test macroexpand-1
	expandMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:swank-macroexpand-1"), slip.String("(double 5)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(3),
	}
	err = swank.WriteWireMessage(conn, expandMsg)
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
		t.Fatalf("expected :return, got %v", response)
	}

	inner, ok := respList[1].(slip.List)
	if !ok || inner[0] != slip.Symbol(":ok") {
		t.Errorf("expected :ok response, got %v", respList[1])
	}

	// The result should contain the expanded form
	result := slip.ObjectString(inner[1])
	if !strings.Contains(result, "*") || !strings.Contains(result, "5") || !strings.Contains(result, "2") {
		t.Errorf("expected expansion to contain '* 5 2', got: %s", result)
	}
}

func TestSlynkMacroexpand(t *testing.T) {
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

	// Test with non-macro form (should return as-is)
	expandMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:swank-macroexpand"), slip.String("(+ 1 2)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, expandMsg)
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
		t.Fatalf("expected :return, got %v", response)
	}

	inner, ok := respList[1].(slip.List)
	if !ok || inner[0] != slip.Symbol(":ok") {
		t.Errorf("expected :ok response")
	}
}

func TestSlynkMacroexpandAll(t *testing.T) {
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

	// Test slynk-macroexpand-all
	expandMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:swank-macroexpand-all"), slip.String("(+ 1 2)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, expandMsg)
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
		t.Fatalf("expected :return, got %v", response)
	}
}

func TestSlynkMacroexpandEmptyArgs(t *testing.T) {
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

	// Test with empty args
	expandMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:swank-macroexpand-1")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, expandMsg)
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
		t.Fatalf("expected :return, got %v", response)
	}
}
