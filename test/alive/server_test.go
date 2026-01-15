// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive_test

import (
	"net"
	"testing"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/alive"
	_ "github.com/ohler55/slip/pkg/cl" // Load CL functions for eval tests
)

func TestServerStartStop(t *testing.T) {
	scope := slip.NewScope()
	server := alive.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start: %v", err)
	}

	if !server.Running() {
		t.Error("server should be running")
	}

	addr := server.Addr()
	if addr == "" {
		t.Error("expected non-empty address")
	}

	err = server.Stop()
	if err != nil {
		t.Errorf("failed to stop: %v", err)
	}

	if server.Running() {
		t.Error("server should not be running")
	}
}

func TestServerDoubleStop(t *testing.T) {
	scope := slip.NewScope()
	server := alive.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start: %v", err)
	}

	_ = server.Stop()
	err = server.Stop()
	if err != nil {
		t.Errorf("second stop should be safe: %v", err)
	}
}

func TestServerConnection(t *testing.T) {
	scope := slip.NewScope()
	server := alive.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start: %v", err)
	}
	defer server.Stop()

	// Connect to server
	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	// Send initialize request
	msg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      1,
		Method:  "initialize",
		Params:  map[string]interface{}{"capabilities": map[string]interface{}{}},
	}
	err = alive.WriteMessage(conn, msg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Read response
	conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := alive.ReadMessage(conn)
	if err != nil {
		t.Fatalf("failed to read response: %v", err)
	}

	if response.Error != nil {
		t.Errorf("unexpected error: %v", response.Error)
	}
	if response.Result == nil {
		t.Error("expected result, got nil")
	}

	// Check capabilities in result
	result, ok := response.Result.(map[string]interface{})
	if !ok {
		t.Fatalf("expected map result, got %T", response.Result)
	}
	if _, ok := result["capabilities"]; !ok {
		t.Error("expected capabilities in result")
	}
}

func TestEvalRequest(t *testing.T) {
	scope := slip.NewScope()
	server := alive.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start: %v", err)
	}
	defer server.Stop()

	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	// Initialize first
	initMsg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      1,
		Method:  "initialize",
		Params:  map[string]interface{}{},
	}
	_ = alive.WriteMessage(conn, initMsg)
	conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	_, _ = alive.ReadMessage(conn)

	// Send eval request
	evalMsg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      2,
		Method:  "$/alive/eval",
		Params: map[string]interface{}{
			"text": "(+ 1 2)",
		},
	}
	err = alive.WriteMessage(conn, evalMsg)
	if err != nil {
		t.Fatalf("failed to write eval: %v", err)
	}

	// Read response
	conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := alive.ReadMessage(conn)
	if err != nil {
		t.Fatalf("failed to read eval response: %v", err)
	}

	if response.Error != nil {
		t.Errorf("unexpected error: %v", response.Error)
	}

	result, ok := response.Result.(map[string]interface{})
	if !ok {
		t.Fatalf("expected map result, got %T", response.Result)
	}

	text, ok := result["text"].(string)
	if !ok || text != "3" {
		t.Errorf("expected text '3', got '%v'", result["text"])
	}
}

func TestListPackages(t *testing.T) {
	scope := slip.NewScope()
	server := alive.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start: %v", err)
	}
	defer server.Stop()

	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	// Initialize first
	initMsg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      1,
		Method:  "initialize",
		Params:  map[string]interface{}{},
	}
	_ = alive.WriteMessage(conn, initMsg)
	conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	_, _ = alive.ReadMessage(conn)

	// Send listPackages request
	pkgMsg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      2,
		Method:  "$/alive/listPackages",
		Params:  map[string]interface{}{},
	}
	err = alive.WriteMessage(conn, pkgMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Read response
	conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := alive.ReadMessage(conn)
	if err != nil {
		t.Fatalf("failed to read response: %v", err)
	}

	if response.Error != nil {
		t.Errorf("unexpected error: %v", response.Error)
	}

	packages, ok := response.Result.([]interface{})
	if !ok {
		t.Fatalf("expected array result, got %T", response.Result)
	}

	if len(packages) == 0 {
		t.Error("expected at least one package")
	}
}
