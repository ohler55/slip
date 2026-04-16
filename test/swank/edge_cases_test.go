// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"bytes"
	"errors"
	"io"
	"net"
	"strings"
	"testing"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/swank"
)

func suppressLog(t *testing.T) {
	t.Helper()
	saved := swank.LogOutput
	swank.LogOutput = io.Discard
	t.Cleanup(func() { swank.LogOutput = saved })
}

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
	if !errors.Is(err, io.EOF) && !errors.Is(err, io.ErrUnexpectedEOF) {
		t.Errorf("expected EOF for incomplete header, got %v", err)
	}
}

func TestWireReadIncompleteBody(t *testing.T) {
	// Test reading a message with complete header but incomplete body
	scope := slip.NewScope()
	// Header says 10 bytes, but only 5 provided
	reader := bytes.NewReader([]byte("00000Ahello"))
	_, err := swank.ReadWireMessage(reader, scope)
	if !errors.Is(err, io.EOF) && !errors.Is(err, io.ErrUnexpectedEOF) {
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
	suppressLog(t)
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
	suppressLog(t)
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
	suppressLog(t)
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
	suppressLog(t)
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

func TestEmacsInterrupt(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	id := env.nextID
	env.nextID++

	// Send long-running eval (don't wait for response)
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:listener-eval"), slip.String("(dotimes (i 100000000) (+ i 1))")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		id,
	}
	err := swank.WriteWireMessage(env.conn, msg)
	if err != nil {
		t.Fatalf("failed to write eval: %v", err)
	}

	// Let eval goroutine start
	time.Sleep(50 * time.Millisecond)

	// Send interrupt
	intMsg := slip.List{
		slip.Symbol(":emacs-interrupt"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(env.conn, intMsg)
	if err != nil {
		t.Fatalf("failed to write interrupt: %v", err)
	}

	// Read response — expect :return :abort
	_, resp := env.readAllResponses(t)
	if resp[0] != slip.Symbol(":return") {
		t.Fatalf("expected :return, got %v", resp[0])
	}
	inner, ok := resp[1].(slip.List)
	if !ok || len(inner) < 2 || inner[0] != slip.Symbol(":abort") {
		t.Fatalf("expected :abort response, got %v", resp[1])
	}
}

// TestWireReadNegativeLength guards against the pre-fix DoS where a
// header like "-0001F" was accepted by strconv.ParseInt and reached
// make([]byte, -31), panicking the goroutine and tearing down the
// process. After the fix, ParseUint rejects the sign and we return
// an error cleanly.
func TestWireReadNegativeLength(t *testing.T) {
	scope := slip.NewScope()
	reader := bytes.NewReader([]byte("-0001F"))
	_, err := swank.ReadWireMessage(reader, scope)
	if err == nil {
		t.Fatal("expected error for negative length header, got nil")
	}
}

// TestWireReadOversizedLength guards against unbounded allocation —
// a header above the 1 MiB sanity cap is rejected before make().
func TestWireReadOversizedLength(t *testing.T) {
	scope := slip.NewScope()
	// 0x200000 = 2 MiB > maxMessageSize (1 MiB); no payload bytes
	// are needed because the guard fires before io.ReadFull.
	reader := bytes.NewReader([]byte("200000"))
	_, err := swank.ReadWireMessage(reader, scope)
	if err == nil {
		t.Fatal("expected error for oversized length header, got nil")
	}
	if !strings.Contains(err.Error(), "exceeds max") {
		t.Fatalf("expected max-size error, got: %v", err)
	}
}

// TestRepeatCreateServerClosesPrior guards bug_009: calling a server
// entry point while another default server is running must stop the
// prior one before overwriting the defaultServer global. Without the
// fix, the first listener leaks (goroutine + port + fd) and is
// unreachable from Lisp.
func TestRepeatCreateServerClosesPrior(t *testing.T) {
	suppressLog(t)
	scope := slip.NewScope()

	first := (&swank.SwankServer{}).Call(scope, slip.List{
		slip.Symbol(":port"), slip.Fixnum(0),
	}, 0).(*swank.SwankServerInstance)
	firstAddr := first.Addr()

	second := (&swank.SwankServer{}).Call(scope, slip.List{
		slip.Symbol(":port"), slip.Fixnum(0),
	}, 0).(*swank.SwankServerInstance)
	t.Cleanup(func() { (&swank.SwankStop{}).Call(scope, nil, 0) })

	if first == second {
		t.Fatal("expected distinct server instances")
	}
	if second.Addr() == firstAddr {
		t.Fatalf("expected second server to bind a different addr, both got %s", firstAddr)
	}

	// The first listener should now be closed — Dial should fail quickly.
	conn, err := net.DialTimeout("tcp", firstAddr, 500*time.Millisecond)
	if err == nil {
		conn.Close()
		t.Fatalf("first listener at %s is still accepting; prior server leaked", firstAddr)
	}
}

func TestEmacsInterruptNoActiveEval(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Send interrupt with no active eval
	msg := slip.List{
		slip.Symbol(":emacs-interrupt"),
		slip.Fixnum(1),
	}
	err := swank.WriteWireMessage(env.conn, msg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Connection should remain functional
	time.Sleep(50 * time.Millisecond)

	// Verify with a normal eval
	resp := env.sendRexOK(t, slip.List{slip.Symbol("swank:connection-info")})
	if resp == nil {
		t.Error("expected connection-info response")
	}
}
