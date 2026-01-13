// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk_test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/slynk"
	"github.com/ohler55/slip/sliptest"
)

func TestSlynkServerStart(t *testing.T) {
	// Test starting a slynk server on a random port
	(&sliptest.Function{
		Source: `(slynk:slynk-server :port 0)`,
		Validate: func(t *testing.T, v slip.Object) {
			str := v.String()
			if !strings.Contains(str, "#<slynk-server") {
				t.Errorf("expected server instance, got %s", str)
			}
		},
	}).Test(t)

	// Stop the server
	(&sliptest.Function{
		Source: `(slynk:slynk-stop)`,
		Expect: "nil",
	}).Test(t)
}

func TestSlynkServerWithHost(t *testing.T) {
	// Test starting with specific host
	(&sliptest.Function{
		Source: `(slynk:slynk-server :port 0 :host "127.0.0.1")`,
		Validate: func(t *testing.T, v slip.Object) {
			str := v.String()
			if !strings.Contains(str, "#<slynk-server") {
				t.Errorf("expected server instance, got %s", str)
			}
		},
	}).Test(t)

	// Stop the server
	(&sliptest.Function{
		Source: `(slynk:slynk-stop)`,
		Expect: "nil",
	}).Test(t)
}

func TestSlynkStopNoServer(t *testing.T) {
	// Stopping when no server is running should be safe
	(&sliptest.Function{
		Source: `(slynk:slynk-stop)`,
		Expect: "nil",
	}).Test(t)
}

func TestSlynkServerInvalidPort(t *testing.T) {
	// Test with invalid port type
	(&sliptest.Function{
		Source: `(slynk:slynk-server :port "not-a-number")`,
		Panics: true,
	}).Test(t)
}

func TestSlynkServerInvalidHost(t *testing.T) {
	// Test with invalid host type
	(&sliptest.Function{
		Source: `(slynk:slynk-server :host 123)`,
		Panics: true,
	}).Test(t)
}

func TestSlynkServerInvalidKeyword(t *testing.T) {
	// Test with invalid keyword
	(&sliptest.Function{
		Source: `(slynk:slynk-server :invalid 123)`,
		Panics: true,
	}).Test(t)
}

func TestSlynkPackageExists(t *testing.T) {
	pkg := slip.FindPackage("slynk")
	if pkg == nil {
		t.Error("slynk package not found")
	}
}

func TestSlynkPortVariable(t *testing.T) {
	v, found := slynk.Pkg.Get("*slynk-port*")
	if !found {
		t.Error("*slynk-port* not found")
	}
	if v != slip.Fixnum(4005) {
		t.Errorf("expected *slynk-port* to be 4005, got %v", v)
	}
}

func TestSlynkServerInstance(t *testing.T) {
	// Start server directly via Go API
	scope := slip.NewScope()
	server := slynk.NewServer(scope)
	err := server.Start(":0")
	if err != nil {
		t.Fatalf("failed to start server: %v", err)
	}
	defer func() { _ = server.Stop() }()

	// Verify server has a valid address
	addr := server.Addr()
	if addr == "" {
		t.Error("expected non-empty server address")
	}
}
