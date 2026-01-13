// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"bytes"
	"testing"

	"github.com/ohler55/slip"
)

func TestWriteWireMessage(t *testing.T) {
	tests := []struct {
		name     string
		msg      slip.Object
		expected string
	}{
		{
			name:     "nil",
			msg:      nil,
			expected: "000003nil",
		},
		{
			name:     "symbol",
			msg:      slip.Symbol(":ok"),
			expected: "000003:ok",
		},
		{
			name:     "simple list",
			msg:      slip.List{slip.Symbol(":return"), slip.Fixnum(42)},
			expected: "00000C(:return 42)",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var buf bytes.Buffer
			err := WriteWireMessage(&buf, tc.msg)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if buf.String() != tc.expected {
				t.Errorf("expected %q, got %q", tc.expected, buf.String())
			}
		})
	}
}

func TestReadWireMessage(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "nil",
			input:    "000003nil",
			expected: "nil",
		},
		{
			name:     "symbol",
			input:    "000003:ok",
			expected: ":ok",
		},
		{
			name:     "list",
			input:    "00000C(:return 42)",
			expected: "(:return 42)",
		},
	}

	scope := slip.NewScope()
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			buf := bytes.NewBufferString(tc.input)
			msg, err := ReadWireMessage(buf, scope)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			result := slip.ObjectString(msg)
			if result != tc.expected {
				t.Errorf("expected %q, got %q", tc.expected, result)
			}
		})
	}
}

func TestWireRoundTrip(t *testing.T) {
	tests := []slip.Object{
		nil,
		slip.Symbol(":emacs-rex"),
		slip.String("hello world"),
		slip.Fixnum(42),
		slip.List{slip.Symbol(":return"), slip.List{slip.Symbol(":ok"), slip.Fixnum(123)}, slip.Fixnum(1)},
	}

	scope := slip.NewScope()
	for _, msg := range tests {
		t.Run(slip.ObjectString(msg), func(t *testing.T) {
			var buf bytes.Buffer
			err := WriteWireMessage(&buf, msg)
			if err != nil {
				t.Fatalf("write error: %v", err)
			}

			result, err := ReadWireMessage(&buf, scope)
			if err != nil {
				t.Fatalf("read error: %v", err)
			}

			if !slip.ObjectEqual(msg, result) {
				t.Errorf("round trip failed: expected %v, got %v", msg, result)
			}
		})
	}
}
