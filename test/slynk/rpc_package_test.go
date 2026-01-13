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

func TestListAllPackages(t *testing.T) {
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
		slip.List{slip.Symbol("slynk:list-all-packages")},
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

	// Check we got :ok with a list
	inner, ok := respList[1].(slip.List)
	if !ok || inner[0] != slip.Symbol(":ok") {
		t.Errorf("expected :ok response")
	}
}

func TestSetPackage(t *testing.T) {
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

	// Set package to common-lisp (full name)
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:set-package"), slip.String("common-lisp")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
	}

	// Read responses until :return (may get :new-package first)
	var gotReturn bool
	for i := 0; i < 5; i++ {
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
		t.Errorf("expected :return response")
	}
}

func TestSetPackageInvalid(t *testing.T) {
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

	// Set package to non-existent
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:set-package"), slip.String("nonexistent-pkg-xyz")},
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

func TestPackageLocalNicknames(t *testing.T) {
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
		slip.List{slip.Symbol("slynk:package-local-nicknames"), slip.String("common-lisp")},
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

func TestPackageLocalNicknamesInvalid(t *testing.T) {
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

	// Query nonexistent package
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:package-local-nicknames"), slip.String("nonexistent")},
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

func TestSlynkRequire(t *testing.T) {
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
		slip.List{slip.Symbol("slynk:slynk-require"), slip.Symbol(":slynk-mrepl")},
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
