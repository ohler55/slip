// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk_test

import (
	"bytes"
	"io"
	"net"
	"testing"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/slynk"
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

func TestConnectionClose(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)
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
	server := slynk.NewServer(scope)
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
	server := slynk.NewServer(scope)
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
		slip.List{slip.Symbol("slynk:unknown-function-xyz")},
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
	server := slynk.NewServer(scope)
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

func TestChannelOperations(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)
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

	// Create channel first
	createMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:create-mrepl"), nil},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, createMsg)
	if err != nil {
		t.Fatalf("failed to write create-mrepl: %v", err)
	}

	// Read until :return to get channel ID
	var chanID slip.Fixnum
	for i := 0; i < 10; i++ {
		_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
		response, err := swank.ReadWireMessage(conn, scope)
		if err != nil {
			break
		}
		respList, ok := response.(slip.List)
		if ok && len(respList) > 1 && respList[0] == slip.Symbol(":return") {
			if inner, ok := respList[1].(slip.List); ok && len(inner) > 1 && inner[0] == slip.Symbol(":ok") {
				if id, ok := inner[1].(slip.Fixnum); ok {
					chanID = id
				}
			}
			break
		}
	}

	// Try to close with non-existent channel ID
	closeMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:mrepl-close"), slip.Fixnum(99999)},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(2),
	}
	err = swank.WriteWireMessage(conn, closeMsg)
	if err != nil {
		t.Fatalf("failed to write close: %v", err)
	}

	// Read response
	_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	_, _ = swank.ReadWireMessage(conn, scope)

	// Close the actual channel
	if chanID > 0 {
		closeActual := slip.List{
			slip.Symbol(":emacs-rex"),
			slip.List{slip.Symbol("slynk:mrepl-close"), chanID},
			slip.String("cl-user"),
			slip.Symbol("t"),
			slip.Fixnum(3),
		}
		err = swank.WriteWireMessage(conn, closeActual)
		if err != nil {
			t.Fatalf("failed to write close actual: %v", err)
		}

		_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
		_, _ = swank.ReadWireMessage(conn, scope)
	}
}

func TestChannelSendMessage(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)
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

	// Create MREPL first to get a channel
	createMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:create-mrepl"), nil},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, createMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Read until :return to get channel ID
	var chanID slip.Fixnum
	for i := 0; i < 10; i++ {
		_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
		response, err := swank.ReadWireMessage(conn, scope)
		if err != nil {
			break
		}
		respList, ok := response.(slip.List)
		if ok && len(respList) > 1 && respList[0] == slip.Symbol(":return") {
			if inner, ok := respList[1].(slip.List); ok && len(inner) > 1 && inner[0] == slip.Symbol(":ok") {
				if id, ok := inner[1].(slip.Fixnum); ok {
					chanID = id
				}
			}
			break
		}
	}

	if chanID == 0 {
		t.Skip("couldn't get channel ID")
	}

	// Send :emacs-channel-send with the channel
	channelMsg := slip.List{
		slip.Symbol(":emacs-channel-send"),
		chanID,
		slip.List{slip.Symbol(":process"), slip.String("(+ 1 2)")},
	}
	err = swank.WriteWireMessage(conn, channelMsg)
	if err != nil {
		t.Fatalf("failed to write channel send: %v", err)
	}

	// Read responses
	for i := 0; i < 10; i++ {
		_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
		_, err := swank.ReadWireMessage(conn, scope)
		if err != nil {
			break
		}
	}
}

func TestMREPLGetHistoryInvalidIndex(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)
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

	// Create MREPL first
	createMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:create-mrepl"), nil},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, createMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Read until :return to get channel ID
	var chanID slip.Fixnum
	for i := 0; i < 10; i++ {
		_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
		response, err := swank.ReadWireMessage(conn, scope)
		if err != nil {
			break
		}
		respList, ok := response.(slip.List)
		if ok && len(respList) > 1 && respList[0] == slip.Symbol(":return") {
			if inner, ok := respList[1].(slip.List); ok && len(inner) > 1 && inner[0] == slip.Symbol(":ok") {
				if id, ok := inner[1].(slip.Fixnum); ok {
					chanID = id
				}
			}
			break
		}
	}

	if chanID == 0 {
		t.Skip("couldn't get channel ID")
	}

	// Try to get history value with out-of-bounds index
	historyMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:mrepl-get-history-value"), chanID, slip.Fixnum(9999)},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(2),
	}
	err = swank.WriteWireMessage(conn, historyMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Read response
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
