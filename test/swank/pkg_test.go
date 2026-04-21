// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/swank"
	"github.com/ohler55/slip/sliptest"
)

func TestSwankServerStart(t *testing.T) {
	// Test starting a swank server on a random port
	(&sliptest.Function{
		Source: `(swank:swank-server :port 0)`,
		Validate: func(t *testing.T, v slip.Object) {
			str := v.String()
			if !strings.Contains(str, "#<swank-server") {
				t.Errorf("expected server instance, got %s", str)
			}
		},
	}).Test(t)

	// Stop the server
	(&sliptest.Function{
		Source: `(swank:swank-stop)`,
		Expect: "nil",
	}).Test(t)
}

func TestSwankServerWithHost(t *testing.T) {
	// Test starting with specific host
	(&sliptest.Function{
		Source: `(swank:swank-server :port 0 :host "127.0.0.1")`,
		Validate: func(t *testing.T, v slip.Object) {
			str := v.String()
			if !strings.Contains(str, "#<swank-server") {
				t.Errorf("expected server instance, got %s", str)
			}
		},
	}).Test(t)

	// Stop the server
	(&sliptest.Function{
		Source: `(swank:swank-stop)`,
		Expect: "nil",
	}).Test(t)
}

func TestSwankStopNoServer(t *testing.T) {
	// Stopping when no server is running should be safe
	(&sliptest.Function{
		Source: `(swank:swank-stop)`,
		Expect: "nil",
	}).Test(t)
}

func TestSwankServerInvalidPort(t *testing.T) {
	// Test with invalid port type
	(&sliptest.Function{
		Source: `(swank:swank-server :port "not-a-number")`,
		Panics: true,
	}).Test(t)
}

func TestSwankServerInvalidHost(t *testing.T) {
	// Test with invalid host type
	(&sliptest.Function{
		Source: `(swank:swank-server :host 123)`,
		Panics: true,
	}).Test(t)
}

func TestSwankServerInvalidKeyword(t *testing.T) {
	// Test with invalid keyword
	(&sliptest.Function{
		Source: `(swank:swank-server :invalid 123)`,
		Panics: true,
	}).Test(t)
}

func TestSwankPackageExists(t *testing.T) {
	pkg := slip.FindPackage("swank")
	if pkg == nil {
		t.Error("swank package not found")
	}
}

func TestSwankPortVariable(t *testing.T) {
	v, found := swank.Pkg.Get("*swank-port*")
	if !found {
		t.Error("*swank-port* not found")
	}
	if v != slip.Fixnum(4005) {
		t.Errorf("expected *swank-port* to be 4005, got %v", v)
	}
}

func TestSwankServerInstance(t *testing.T) {
	// Start server directly via Go API
	scope := slip.NewScope()
	server := swank.NewServer(scope)
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
