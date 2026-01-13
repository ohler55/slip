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

func TestFlexCompletions(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

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

	// Test flex completions for "car"
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:flex-completions"), slip.String("car"), slip.String("cl-user")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
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
		t.Fatalf("expected :return, got %v", response)
	}

	// Check we got :ok
	inner, ok := respList[1].(slip.List)
	if !ok || inner[0] != slip.Symbol(":ok") {
		t.Errorf("expected :ok response")
	}
}

func TestCompletions(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

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

	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:completions"), slip.String("def"), slip.String("cl-user")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
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

func TestSimpleCompletions(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

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

	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:simple-completions"), slip.String("ma"), slip.String("cl-user")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
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

func TestApropos(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

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

	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:apropos-list-for-emacs"), slip.String("car")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
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

func TestFlexCompletionsEmptyPattern(t *testing.T) {
	scope := slip.NewScope()
	server := slynk.NewServer(scope)

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

	// Test with empty pattern
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:flex-completions"), slip.String(""), slip.String("cl-user")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
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

func TestFlexScoreEdgeCases(t *testing.T) {
	tests := []struct {
		name    string
		pattern string
		wantHit bool
	}{
		// Empty cases
		{"", "", false},
		{"car", "", false},
		// Pattern longer than name
		{"ab", "abc", false},
		// Exact matches
		{"a", "a", true},
		{"car", "car", true},
		// Prefix matches
		{"car", "c", true},
		{"car", "ca", true},
		// Non-contiguous matches
		{"car", "cr", true},
		{"cadr", "cd", true},
		// Word boundary matches
		{"make-array", "ma", true},
		{"make-array", "m-a", true},
		// No match
		{"car", "xyz", false},
		{"car", "d", false},
	}

	for _, tt := range tests {
		score, _ := slynk.FlexScore(tt.name, tt.pattern)
		gotHit := score > 0
		if gotHit != tt.wantHit {
			t.Errorf("FlexScore(%q, %q) hit=%v, want hit=%v (score=%f)",
				tt.name, tt.pattern, gotHit, tt.wantHit, score)
		}
	}
}
