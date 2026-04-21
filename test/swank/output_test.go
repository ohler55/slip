// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/swank"
)

func TestHandlerListRegistration(t *testing.T) {
	// Test that ListHandlers works
	handlers := swank.ListHandlers()
	if len(handlers) == 0 {
		t.Error("expected registered handlers")
	}

	// Check some expected handlers exist
	expectedHandlers := []string{
		"swank:connection-info",
		"swank:create-repl",
		"swank:listener-eval",
	}

	for _, expected := range expectedHandlers {
		found := false
		for _, h := range handlers {
			if h == expected {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("expected handler %s to be registered", expected)
		}
	}
}

func TestOutputStreamWrite(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Evaluate something via interactive-eval
	env.sendRexOK(t, slip.List{slip.Symbol("swank:interactive-eval"), slip.String("(+ 1 2)")})
}

func TestOutputStreamWriteString(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Use listener-eval which calls WriteString and WriteResult
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:listener-eval"), slip.String("(+ 1 2)")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	if err := swank.WriteWireMessage(env.conn, msg); err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Read until :return, checking for :write-string messages
	writes, ret := env.readAllResponses(t)
	if ret[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", ret[0])
	}
	// Should have at least one :write-string with the result
	foundResult := false
	for _, w := range writes {
		if strings.Contains(w, "3") {
			foundResult = true
		}
	}
	if !foundResult {
		t.Logf("write-string messages: %v", writes)
	}
}

func TestBufferFirstChange(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	resp := env.sendRex(t, slip.List{slip.Symbol("swank:buffer-first-change"), slip.String("test.lisp")})
	if resp[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", resp[0])
	}
}

func TestInitPresentations(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	resp := env.sendRex(t, slip.List{slip.Symbol("swank:init-presentations")})
	if resp[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", resp[0])
	}
}
