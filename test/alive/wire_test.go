// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/slip/pkg/alive"
)

func TestWriteReadMessage(t *testing.T) {
	// Test round-trip
	original := &alive.JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      1,
		Method:  "initialize",
		Params:  map[string]interface{}{"capabilities": map[string]interface{}{}},
	}

	var buf bytes.Buffer
	err := alive.WriteMessage(&buf, original)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	result, err := alive.ReadMessage(&buf)
	if err != nil {
		t.Fatalf("failed to read: %v", err)
	}

	if result.JSONRPC != "2.0" {
		t.Errorf("expected jsonrpc 2.0, got %s", result.JSONRPC)
	}
	if result.ID != float64(1) { // JSON numbers unmarshal as float64
		t.Errorf("expected id 1, got %v", result.ID)
	}
	if result.Method != "initialize" {
		t.Errorf("expected method initialize, got %s", result.Method)
	}
}

func TestWriteReadResponse(t *testing.T) {
	original := alive.NewResponse(42, map[string]interface{}{
		"capabilities": map[string]interface{}{
			"hoverProvider": true,
		},
	})

	var buf bytes.Buffer
	err := alive.WriteMessage(&buf, original)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	result, err := alive.ReadMessage(&buf)
	if err != nil {
		t.Fatalf("failed to read: %v", err)
	}

	if result.ID != float64(42) {
		t.Errorf("expected id 42, got %v", result.ID)
	}
	if result.Result == nil {
		t.Error("expected result, got nil")
	}
}

func TestWriteReadError(t *testing.T) {
	original := alive.NewErrorResponse(1, alive.MethodNotFound, "unknown method", nil)

	var buf bytes.Buffer
	err := alive.WriteMessage(&buf, original)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	result, err := alive.ReadMessage(&buf)
	if err != nil {
		t.Fatalf("failed to read: %v", err)
	}

	if result.Error == nil {
		t.Fatal("expected error, got nil")
	}
	if result.Error.Code != alive.MethodNotFound {
		t.Errorf("expected code %d, got %d", alive.MethodNotFound, result.Error.Code)
	}
}

func TestReadInvalidJSON(t *testing.T) {
	buf := bytes.NewBufferString("Content-Length: 5\r\n\r\nhello")
	_, err := alive.ReadMessage(buf)
	if err == nil {
		t.Error("expected error for invalid JSON")
	}
}

func TestReadMissingContentLength(t *testing.T) {
	buf := bytes.NewBufferString("\r\n{}")
	_, err := alive.ReadMessage(buf)
	if err == nil {
		t.Error("expected error for missing Content-Length")
	}
}
