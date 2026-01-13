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

func TestCreateMREPL(t *testing.T) {
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

	// Create MREPL
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:create-mrepl"), nil},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
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
		t.Errorf("expected :return response")
	}
}

func TestMREPLNew(t *testing.T) {
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
		slip.List{slip.Symbol("slynk:mrepl-new")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
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
		t.Errorf("expected :return response")
	}
}

func TestMREPLClose(t *testing.T) {
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

	// First create an MREPL to get a channel ID
	createMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:mrepl-new")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	_ = swank.WriteWireMessage(conn, createMsg)

	// Read until we get the channel ID from :return
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

	// Now close the channel
	closeMsg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:mrepl-close"), chanID},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(2),
	}
	err = swank.WriteWireMessage(conn, closeMsg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
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
		t.Errorf("expected :return response")
	}
}

func TestListChannels(t *testing.T) {
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
		slip.List{slip.Symbol("slynk:list-channels")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
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
		t.Errorf("expected :return response")
	}
}

func TestGetHistoryValue(t *testing.T) {
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

	// Test with invalid channel ID
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:mrepl-get-history-value"), slip.Fixnum(999), slip.Fixnum(0)},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
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
		t.Errorf("expected :return response")
	}
}

func TestMREPLCloseInvalidArgs(t *testing.T) {
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

	// Try to close without channel ID
	msg := slip.List{
		slip.Symbol(":emacs-rex"),
		slip.List{slip.Symbol("slynk:mrepl-close")},
		slip.String("cl-user"),
		slip.Symbol("t"),
		slip.Fixnum(1),
	}
	err = swank.WriteWireMessage(conn, msg)
	if err != nil {
		t.Fatalf("failed to write: %v", err)
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
		t.Errorf("expected :return response")
	}
}
