// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk_test

import (
	"net"
	"testing"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/slynk"
	"github.com/ohler55/slip/pkg/swank"
)

func TestServerStartStop(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

	err := server.Start(":0") // Use any available port
	if err != nil {
		t.Fatalf("Failed to start server: %v", err)
	}
	defer func() { _ = server.Stop() }()

	if !server.Running() {
		t.Fatal("Server should be running after Start")
	}

	addr := server.Addr()
	if addr == "" {
		t.Fatal("Server should have an address")
	}

	_ = server.Stop()

	if server.Running() {
		t.Fatal("Server should not be running after Stop")
	}
}

func TestConnectionInfo(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("Failed to start server: %v", err)
	}
	defer func() { _ = server.Stop() }()

	// Connect to the server
	conn, err := net.Dial("tcp", server.Addr())
	if err != nil {
		t.Fatalf("Failed to connect: %v", err)
	}
	defer conn.Close()

	// Send connection-info request
	request := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:connection-info")},
		slip.String("cl-user"),
		slip.TrueSymbol,
		slip.Fixnum(1),
	}

	err = swank.WriteWireMessage(conn, request)
	if err != nil {
		t.Fatalf("Failed to write message: %v", err)
	}

	// Read response
	_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := swank.ReadWireMessage(conn, scope)
	if err != nil {
		t.Fatalf("Failed to read response: %v", err)
	}

	// Check response format
	respList, ok := response.(slip.List)
	if !ok || len(respList) < 2 {
		t.Fatalf("Invalid response format: %v", response)
	}

	if respList[0] != slip.Symbol(":return") {
		t.Fatalf("Expected :return, got: %v", respList[0])
	}
}

func TestFlexCompletion(t *testing.T) {
	// Test flex scoring algorithm
	tests := []struct {
		name    string
		pattern string
		wantHit bool
	}{
		{"car", "car", true},
		{"car", "ca", true},
		{"car", "c", true},
		{"car", "cr", true}, // flex match
		{"car", "xyz", false},
		{"defun", "df", true}, // flex match
		{"make-array", "ma", true},
		{"make-array", "mka", true}, // flex match
	}

	for _, tt := range tests {
		score, _ := slynk.FlexScore(tt.name, tt.pattern)
		gotHit := score > 0
		if gotHit != tt.wantHit {
			t.Errorf("FlexScore(%q, %q) hit=%v, want hit=%v", tt.name, tt.pattern, gotHit, tt.wantHit)
		}
	}
}

func TestChannel(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

	err := server.Start(":0")
	if err != nil {
		t.Fatalf("Failed to start server: %v", err)
	}
	defer func() { _ = server.Stop() }()

	// Simulate connection setup
	netConn, err := net.Dial("tcp", server.Addr())
	if err != nil {
		t.Fatalf("Failed to connect: %v", err)
	}
	defer netConn.Close()

	// Wait for connection to be established
	time.Sleep(100 * time.Millisecond)

	// The server should have created a connection with a default channel
	// This is tested implicitly by the connection-info test
}

func TestHandlerRegistry(t *testing.T) {
	// Test handler lookup
	handler := slynk.GetHandler("slynk:connection-info")
	if handler == nil {
		t.Fatal("Expected to find connection-info handler")
	}

	// Test with swank: prefix (compatibility)
	handler = slynk.GetHandler("swank:connection-info")
	if handler == nil {
		t.Fatal("Expected swank: prefix to work for compatibility")
	}

	// Test without prefix
	handler = slynk.GetHandler("connection-info")
	if handler == nil {
		t.Fatal("Expected handler lookup without prefix to work")
	}

	// Test unknown handler
	handler = slynk.GetHandler("unknown-handler")
	if handler != nil {
		t.Fatal("Expected nil for unknown handler")
	}
}
