// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"io"
	"net"
	"testing"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/swank"
)

// testEnv holds a running server and connection for a test.
type testEnv struct {
	server         *swank.Server
	conn           net.Conn
	scope          *slip.Scope
	nextID         slip.Fixnum
	savedLogOutput io.Writer
}

// newTestEnv starts a swank server, connects to it, and returns the env.
// Caller must defer env.close().
func newTestEnv(t *testing.T) *testEnv {
	t.Helper()
	saved := swank.LogOutput
	swank.LogOutput = io.Discard
	scope := slip.NewScope()
	server := swank.NewServer(scope)
	if err := server.Start(":0"); err != nil {
		swank.LogOutput = saved
		t.Fatalf("failed to start server: %v", err)
	}
	conn, err := net.DialTimeout("tcp", server.Addr(), time.Second)
	if err != nil {
		swank.LogOutput = saved
		_ = server.Stop()
		t.Fatalf("failed to connect: %v", err)
	}
	return &testEnv{server: server, conn: conn, scope: scope, nextID: 1, savedLogOutput: saved}
}

func (e *testEnv) close() {
	e.conn.Close()
	_ = e.server.Stop()
	swank.LogOutput = e.savedLogOutput
}

// sendRex sends an :emacs-rex message and reads the response.
func (e *testEnv) sendRex(t *testing.T, form slip.List) slip.List {
	t.Helper()
	id := e.nextID
	e.nextID++
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		form,
		slip.String("cl-user"),
		slip.Symbol("t"),
		id,
	}
	if err := swank.WriteWireMessage(e.conn, msg); err != nil {
		t.Fatalf("failed to write: %v", err)
	}
	_ = e.conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	response, err := swank.ReadWireMessage(e.conn, e.scope)
	if err != nil {
		t.Fatalf("failed to read response: %v", err)
	}
	respList, ok := response.(slip.List)
	if !ok || len(respList) < 2 {
		t.Fatalf("invalid response: %v", response)
	}
	return respList
}

// sendRexOK sends an :emacs-rex and asserts :return :ok, returning the result value.
func (e *testEnv) sendRexOK(t *testing.T, form slip.List) slip.Object {
	t.Helper()
	resp := e.sendRex(t, form)
	if resp[0] != slip.Symbol(":return") {
		t.Fatalf("expected :return, got %v", resp[0])
	}
	inner, ok := resp[1].(slip.List)
	if !ok || len(inner) < 1 || inner[0] != slip.Symbol(":ok") {
		t.Fatalf("expected :ok, got %v", resp[1])
	}
	if len(inner) > 1 {
		return inner[1]
	}
	return nil
}

// readAllResponses reads responses until :return, collecting :write-string messages.
func (e *testEnv) readAllResponses(t *testing.T) (writes []string, returnResp slip.List) {
	t.Helper()
	_ = e.conn.SetReadDeadline(time.Now().Add(5 * time.Second))
	for i := 0; i < 20; i++ {
		response, err := swank.ReadWireMessage(e.conn, e.scope)
		if err != nil {
			break
		}
		respList, ok := response.(slip.List)
		if !ok || len(respList) == 0 {
			continue
		}
		if respList[0] == slip.Symbol(":write-string") && len(respList) > 1 {
			if s, ok := respList[1].(slip.String); ok {
				writes = append(writes, string(s))
			}
		}
		if respList[0] == slip.Symbol(":return") {
			return writes, respList
		}
	}
	t.Fatal("never received :return response")
	return nil, nil
}
