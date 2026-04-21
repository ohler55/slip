// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"net"
	"testing"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/swank"
)

func TestCompletions(t *testing.T) {
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

	// Test completions for "car"
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:completions"), slip.String("car"), slip.String("cl-user")},
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

func TestSimpleCompletions(t *testing.T) {
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

	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:simple-completions"), slip.String("def"), slip.String("cl-user")},
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

func TestFuzzyCompletions(t *testing.T) {
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

	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:fuzzy-completions"), slip.String("ma"), slip.String("cl-user")},
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

	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:apropos-list-for-emacs"), slip.String("car")},
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

func TestCompletionsForKeyword(t *testing.T) {
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

	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:completions-for-keyword"), slip.String(":test")},
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

func TestCompletionsEmptyPrefix(t *testing.T) {
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

	// Test with empty prefix
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("swank:completions"), slip.String(""), slip.String("cl-user")},
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
