// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive_test

import (
	"net"
	"strings"
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

func TestCompletion(t *testing.T) {
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

	// Send completion request with prefix "car"
	compMsg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      2,
		Method:  "textDocument/completion",
		Params: map[string]interface{}{
			"word": "car",
		},
	}
	err = alive.WriteMessage(conn, compMsg)
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

	result, ok := response.Result.(map[string]interface{})
	if !ok {
		t.Fatalf("expected map result, got %T", response.Result)
	}

	items, ok := result["items"].([]interface{})
	if !ok {
		t.Fatalf("expected items array, got %T", result["items"])
	}

	// Should find car function
	found := false
	for _, item := range items {
		if m, ok := item.(map[string]interface{}); ok {
			if m["label"] == "car" {
				found = true
				break
			}
		}
	}
	if !found {
		t.Error("expected to find 'car' in completions")
	}
}

func TestHover(t *testing.T) {
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

	// Send hover request for "car"
	hoverMsg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      2,
		Method:  "textDocument/hover",
		Params: map[string]interface{}{
			"word": "car",
		},
	}
	err = alive.WriteMessage(conn, hoverMsg)
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

	result, ok := response.Result.(map[string]interface{})
	if !ok {
		t.Fatalf("expected map result, got %T", response.Result)
	}

	contents, ok := result["contents"].(map[string]interface{})
	if !ok {
		t.Fatalf("expected contents map, got %T", result["contents"])
	}

	if contents["kind"] != "markdown" {
		t.Errorf("expected markdown kind, got %v", contents["kind"])
	}

	value, ok := contents["value"].(string)
	if !ok || value == "" {
		t.Error("expected non-empty hover value")
	}

	// Should contain CAR in the description
	if !strings.Contains(value, "CAR") {
		t.Errorf("expected hover to contain CAR, got: %s", value)
	}
}

func TestHoverNoWord(t *testing.T) {
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

	// Send hover request without word
	hoverMsg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      2,
		Method:  "textDocument/hover",
		Params:  map[string]interface{}{},
	}
	err = alive.WriteMessage(conn, hoverMsg)
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

	// Result should be nil when no word provided
	if response.Result != nil {
		t.Errorf("expected nil result for missing word, got %v", response.Result)
	}
}

func TestMacroexpand1(t *testing.T) {
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

	// First, define a simple macro
	defMsg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      2,
		Method:  "$/alive/eval",
		Params: map[string]interface{}{
			"text": "(defmacro double (x) `(* ,x 2))",
		},
	}
	_ = alive.WriteMessage(conn, defMsg)
	conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	_, _ = alive.ReadMessage(conn)

	// Now test macroexpand-1
	expandMsg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      3,
		Method:  "$/alive/macroexpand1",
		Params: map[string]interface{}{
			"text": "(double 5)",
		},
	}
	err = alive.WriteMessage(conn, expandMsg)
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

	result, ok := response.Result.(map[string]interface{})
	if !ok {
		t.Fatalf("expected map result, got %T", response.Result)
	}

	text, ok := result["text"].(string)
	if !ok || text == "" {
		t.Error("expected non-empty text in result")
	}

	// Should expand to (* 5 2) or similar
	if !strings.Contains(text, "*") || !strings.Contains(text, "5") || !strings.Contains(text, "2") {
		t.Errorf("expected expansion to contain '* 5 2', got: %s", text)
	}
}

func TestMacroexpand(t *testing.T) {
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

	// Test with a standard macro like 'when'
	expandMsg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      2,
		Method:  "$/alive/macroexpand",
		Params: map[string]interface{}{
			"text": "(when t (print 'hello))",
		},
	}
	err = alive.WriteMessage(conn, expandMsg)
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

	result, ok := response.Result.(map[string]interface{})
	if !ok {
		t.Fatalf("expected map result, got %T", response.Result)
	}

	text, ok := result["text"].(string)
	if !ok || text == "" {
		t.Error("expected non-empty text in result")
	}

	// 'when' typically expands to 'if' or 'progn'
	// Just check we got something back
	t.Logf("macroexpand result: %s", text)
}

func TestMacroexpandNonMacro(t *testing.T) {
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

	// Test with a regular function (not a macro)
	expandMsg := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      2,
		Method:  "$/alive/macroexpand1",
		Params: map[string]interface{}{
			"text": "(+ 1 2)",
		},
	}
	err = alive.WriteMessage(conn, expandMsg)
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

	result, ok := response.Result.(map[string]interface{})
	if !ok {
		t.Fatalf("expected map result, got %T", response.Result)
	}

	text, ok := result["text"].(string)
	if !ok {
		t.Error("expected text in result")
	}

	// Should return the original form unchanged
	if !strings.Contains(text, "+") && !strings.Contains(text, "1") && !strings.Contains(text, "2") {
		t.Errorf("expected original form '(+ 1 2)', got: %s", text)
	}
}
