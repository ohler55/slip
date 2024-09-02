// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"net"
	"net/http"
	"net/http/httptest"
	"strings"
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestGetPeerAddressOkay(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (get-peer-address sock))`,
		Expect: `""`,
	}).Test(t)
}

func TestUsocketPeerAddressOkay(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'usocket :socket ufd)))
                  (send sock :peer-address))`,
		Expect: `""`,
	}).Test(t)
}

func TestGetPeerAddressHTTP(t *testing.T) {
	serv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("okay"))
	}))
	defer serv.Close()

	serv.Config.ConnState = func(nc net.Conn, cs http.ConnState) {
		if cs == http.StateActive {
			addr := nc.RemoteAddr().String()
			pos := strings.LastIndexByte(addr, ':')
			address := addr[:pos]
			scope := slip.NewScope()
			us := slip.ReadString("(make-instance 'usocket)").Eval(scope, nil).(*flavors.Instance)
			us.Any = nc
			scope.Let(slip.Symbol("sock"), us)
			result := slip.ReadString("(send sock :peer-address)").Eval(scope, nil).(slip.String)
			tt.Equal(t, slip.String(address), result)
		}
	}
	if resp, err := serv.Client().Get(serv.URL); err == nil {
		_ = resp.Body.Close()
	}
}

func TestGetPeerAddressNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(get-peer-address t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestUsocketPeerAddressClosed(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance 'usocket) :peer-address)`,
		Expect: "nil",
	}).Test(t)
}
