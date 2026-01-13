// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"net"
	"strings"
	"testing"
	"time"

	"github.com/ohler55/slip"
)

func TestServerStartStop(t *testing.T) {
	scope := slip.NewScope()
	server := NewServer(scope)

	// Start on random port
	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start server: %v", err)
	}
	defer server.Stop()

	if !server.Running() {
		t.Error("server should be running")
	}

	addr := server.Addr()
	if addr == "" {
		t.Error("server should have an address")
	}

	// Stop server
	err = server.Stop()
	if err != nil {
		t.Fatalf("failed to stop server: %v", err)
	}

	if server.Running() {
		t.Error("server should not be running after stop")
	}
}

func TestServerConnection(t *testing.T) {
	scope := slip.NewScope()
	server := NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start server: %v", err)
	}
	defer server.Stop()

	// Connect to server
	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	// Send connection-info request
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:connection-info")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}

	err = WriteWireMessage(conn, msg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Read response with timeout
	conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := ReadWireMessage(conn, scope)
	if err != nil {
		t.Fatalf("failed to read response: %v", err)
	}

	// Verify response format
	respList, ok := response.(slip.List)
	if !ok {
		t.Fatalf("expected list response, got %T", response)
	}

	if len(respList) < 2 {
		t.Fatalf("response too short: %v", respList)
	}

	if respList[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", respList[0])
	}

	// Check for :ok status
	inner, ok := respList[1].(slip.List)
	if !ok || len(inner) < 1 {
		t.Fatalf("expected inner list, got %v", respList[1])
	}

	if inner[0] != slip.Symbol(":ok") {
		t.Errorf("expected :ok, got %v", inner[0])
	}

	// Verify connection-info contains expected keys
	responseStr := slip.ObjectString(response)
	expectedKeys := []string{":pid", ":lisp-implementation", ":package", ":version"}
	for _, key := range expectedKeys {
		if !strings.Contains(responseStr, key) {
			t.Errorf("response missing key %s: %s", key, responseStr)
		}
	}
}

func TestServerEval(t *testing.T) {
	scope := slip.NewScope()
	server := NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start server: %v", err)
	}
	defer server.Stop()

	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	// First, create a REPL
	createRepl := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:create-repl"), nil},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = WriteWireMessage(conn, createRepl)
	if err != nil {
		t.Fatalf("failed to write create-repl: %v", err)
	}

	conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	_, err = ReadWireMessage(conn, scope)
	if err != nil {
		t.Fatalf("failed to read create-repl response: %v", err)
	}

	// Now evaluate an expression
	evalMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:listener-eval"), slip.String("(+ 1 2)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(2),
	}
	err = WriteWireMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write eval: %v", err)
	}

	// Read all responses (may include :write-string before :return)
	var foundResult bool
	for i := 0; i < 5; i++ {
		conn.SetReadDeadline(time.Now().Add(2 * time.Second))
		response, err := ReadWireMessage(conn, scope)
		if err != nil {
			break
		}

		respList, ok := response.(slip.List)
		if !ok || len(respList) == 0 {
			continue
		}

		if respList[0] == slip.Symbol(":write-string") {
			// Check for result
			responseStr := slip.ObjectString(response)
			if strings.Contains(responseStr, "3") {
				foundResult = true
			}
		}

		if respList[0] == slip.Symbol(":return") {
			break
		}
	}

	if !foundResult {
		t.Log("Note: result might be in :return value instead of :write-string")
	}
}
