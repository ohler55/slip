// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"bytes"
	"fmt"
	"io"
	"net"
	"strconv"
	"strings"
	"sync"
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

// TestConcurrentEvalOutputsIsolated guards bug_003: concurrent
// :emacs-rex handlers used to race on the package-global
// slip.StandardOutput. Each goroutine runs a distinct eval whose
// result contains its own numeric tag. Under `go test -race`, the
// underlying data race on slip.StandardOutput is also caught.
//
// NOTE: we deliberately avoid string literals inside the eval
// source because slip's wire format does not escape inner quotes —
// a round-tripped payload `"(princ \"x\")"` is re-parsed as four
// tokens, not one string. Integer tags widely spaced (100 apart)
// avoid substring collisions.
func TestConcurrentEvalOutputsIsolated(t *testing.T) {
	saved := swank.LogOutput
	swank.LogOutput = io.Discard
	defer func() { swank.LogOutput = saved }()

	const n = 8
	scope := slip.NewScope()
	server := swank.NewServer(scope)
	if err := server.Start(":0"); err != nil {
		t.Fatalf("failed to start server: %v", err)
	}
	defer func() { _ = server.Stop() }()

	// Pre-serialize each goroutine's wire payload so slip's Printer
	// (which is not concurrency-safe for a single shared instance) is
	// only exercised from this goroutine. The server-side decode is
	// the thing we want to race, not the client-side encode.
	payloads := make([][]byte, n)
	tags := make([]int, n)
	for i := 0; i < n; i++ {
		tag := (i + 1) * 100 // 100, 200, ..., 800
		tags[i] = tag
		source := fmt.Sprintf("(princ %d)", tag)
		msg := slip.List{
			slip.Symbol(":emacs-rex"),
			slip.List{slip.Symbol("swank:interactive-eval"), slip.String(source)},
			slip.String("cl-user"),
			slip.Symbol("t"),
			slip.Fixnum(int64(i + 1)),
		}
		var buf bytes.Buffer
		if err := swank.WriteWireMessage(&buf, msg); err != nil {
			t.Fatalf("pre-encode %d: %v", i, err)
		}
		payloads[i] = buf.Bytes()
	}

	var wg sync.WaitGroup
	errs := make(chan error, n)

	for i := 0; i < n; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()

			conn, err := net.DialTimeout("tcp", server.Addr(), 2*time.Second)
			if err != nil {
				errs <- fmt.Errorf("dial: %w", err)
				return
			}
			defer conn.Close()

			if _, err := conn.Write(payloads[id]); err != nil {
				errs <- fmt.Errorf("write: %w", err)
				return
			}

			_ = conn.SetReadDeadline(time.Now().Add(5 * time.Second))
			resp, err := swank.ReadWireMessage(conn, scope)
			if err != nil {
				errs <- fmt.Errorf("conn %d: read: %w", id, err)
				return
			}
			list, ok := resp.(slip.List)
			if !ok || len(list) < 2 || list[0] != slip.Symbol(":return") {
				errs <- fmt.Errorf("conn %d: bad response: %v", id, resp)
				return
			}
			inner, ok := list[1].(slip.List)
			if !ok || len(inner) < 2 || inner[0] != slip.Symbol(":ok") {
				errs <- fmt.Errorf("conn %d: not :ok: %v", id, list[1])
				return
			}
			result, ok := inner[1].(slip.String)
			if !ok {
				errs <- fmt.Errorf("conn %d: result not string: %v", id, inner[1])
				return
			}
			want := strconv.Itoa(tags[id])
			if !strings.Contains(string(result), want) {
				errs <- fmt.Errorf("conn %d: missing own tag %s in result %q", id, want, result)
				return
			}
		}(i)
	}

	wg.Wait()
	close(errs)
	for err := range errs {
		t.Error(err)
	}
}

// TestListenerEvalEmptySource covers the len(code) == 0 early return
// in evalWithCapture: an empty source string must not panic and must
// yield a successful :return to SLIME.
func TestListenerEvalEmptySource(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	source := ""

	// listener-eval routes through evalWithCapture with sendOutput=true,
	// so we read responses via readAllResponses (handles :write-string +
	// :return framing).
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:listener-eval"), slip.String(source)},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	if err := swank.WriteWireMessage(env.conn, msg); err != nil {
		t.Fatalf("write: %v", err)
	}
	_, ret := env.readAllResponses(t)
	if ret[0] != slip.Symbol(":return") {
		t.Fatalf("expected :return, got %v", ret[0])
	}
	inner, ok := ret[1].(slip.List)
	if !ok || len(inner) < 1 || inner[0] != slip.Symbol(":ok") {
		t.Fatalf("expected :ok, got %v", ret[1])
	}
}

// TestListenerEvalErrorPath covers the slip.Panic recovery branch in
// evalWithCapture with sendOutput=true: the panic is swallowed, an
// "Error: ..." :write-string is sent, and the :return is :ok with nil
// (handleEmacsRex only produces :abort for panics that escape
// evalWithCapture's own recover).
func TestListenerEvalErrorPath(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	source := "(car 5)"

	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:listener-eval"), slip.String(source)},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	if err := swank.WriteWireMessage(env.conn, msg); err != nil {
		t.Fatalf("write: %v", err)
	}
	writes, ret := env.readAllResponses(t)

	// Substring match on "Error:" — don't couple to the CL error text
	// (which may change); require a :write-string with that prefix.
	sawError := false
	for _, w := range writes {
		if strings.Contains(w, "Error:") {
			sawError = true
			break
		}
	}
	if !sawError {
		t.Errorf("expected an 'Error:' :write-string, got writes=%v", writes)
	}

	if ret[0] != slip.Symbol(":return") {
		t.Fatalf("expected :return, got %v", ret[0])
	}
	inner, ok := ret[1].(slip.List)
	if !ok || len(inner) < 1 || inner[0] != slip.Symbol(":ok") {
		t.Fatalf("expected :ok (eval error swallowed into :ok nil), got %v", ret[1])
	}
}
