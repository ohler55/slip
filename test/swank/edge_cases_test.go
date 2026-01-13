// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"bytes"
	"io"
	"net"
	"testing"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/swank"
)

func TestWireReadInvalidHeader(t *testing.T) {
	// Test reading a message with invalid header (non-hex characters)
	scope := slip.NewScope()
	reader := bytes.NewReader([]byte("ZZZZZZ"))
	_, err := swank.ReadWireMessage(reader, scope)
	if err == nil {
		t.Error("expected error for invalid hex header")
	}
}

func TestWireReadIncompleteHeader(t *testing.T) {
	// Test reading a message with incomplete header
	scope := slip.NewScope()
	reader := bytes.NewReader([]byte("000"))
	_, err := swank.ReadWireMessage(reader, scope)
	if err != io.EOF && err != io.ErrUnexpectedEOF {
		t.Errorf("expected EOF for incomplete header, got %v", err)
	}
}

func TestWireReadIncompleteBody(t *testing.T) {
	// Test reading a message with complete header but incomplete body
	scope := slip.NewScope()
	// Header says 10 bytes, but only 5 provided
	reader := bytes.NewReader([]byte("00000Ahello"))
	_, err := swank.ReadWireMessage(reader, scope)
	if err != io.EOF && err != io.ErrUnexpectedEOF {
		t.Errorf("expected EOF for incomplete body, got %v", err)
	}
}

func TestWireWriteMessage(t *testing.T) {
	// Test writing a valid message
	var buf bytes.Buffer
	msg := slip.List{slip.Symbol(":test"), slip.Fixnum(123)}
	err := swank.WriteWireMessage(&buf, msg)
	if err != nil {
		t.Fatalf("failed to write message: %v", err)
	}

	// Verify format: 6 hex chars + payload
	data := buf.Bytes()
	if len(data) < 6 {
		t.Fatal("output too short")
	}
}

func TestWireRoundTrip(t *testing.T) {
	// Test write then read
	original := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:connection-info")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}

	var buf bytes.Buffer
	err := swank.WriteWireMessage(&buf, original)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	scope := slip.NewScope()
	result, err := swank.ReadWireMessage(&buf, scope)
	if err != nil {
		t.Fatalf("failed to read: %v", err)
	}

	resultList, ok := result.(slip.List)
	if !ok {
		t.Fatalf("expected List, got %T", result)
	}
	if len(resultList) != 5 {
		t.Errorf("expected 5 elements, got %d", len(resultList))
	}
}

func TestConnectionClose(t *testing.T) {
	scope := slip.NewScope()
	server := swank.NewServer(scope)
	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start: %v", err)
	}
	defer func() { _ = server.Stop() }()

	// Connect then immediately close
	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	_ = conn.Close()

	// Give server time to handle disconnection
	time.Sleep(50 * time.Millisecond)
}

func TestServerDoubleStop(t *testing.T) {
	scope := slip.NewScope()
	server := swank.NewServer(scope)
	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start: %v", err)
	}

	// First stop should succeed
	err = server.Stop()
	if err != nil {
		t.Errorf("first stop failed: %v", err)
	}

	// Second stop should be safe
	err = server.Stop()
	if err != nil {
		t.Errorf("second stop should be safe, got: %v", err)
	}
}

func TestUnknownRPCFunction(t *testing.T) {
	scope := slip.NewScope()
	server := swank.NewServer(scope)
	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start: %v", err)
	}
	defer func() { _ = server.Stop() }()

	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	// Send unknown RPC call
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:unknown-function-xyz")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Should get a response (either error or abort)
	_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := swank.ReadWireMessage(conn, scope)
	if err != nil {
		t.Fatalf("expected response, got error: %v", err)
	}

	respList, ok := response.(slip.List)
	if !ok || len(respList) == 0 {
		t.Errorf("expected list response, got %T", response)
	}
}

func TestMalformedEmacsRex(t *testing.T) {
	scope := slip.NewScope()
	server := swank.NewServer(scope)
	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start: %v", err)
	}
	defer func() { _ = server.Stop() }()

	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		t.Fatalf("failed to connect: %v", err)
	}
	defer conn.Close()

	// Send malformed :emacs-rex (missing required elements)
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		// Missing form, package, thread, continuation
	}
	err = swank.WriteWireMessage(conn, msg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Connection should remain functional or gracefully close
	time.Sleep(50 * time.Millisecond)
}
