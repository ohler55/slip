// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"net"
	"net/http"
	"net/http/httptest"
	"net/url"
	"strconv"
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestGetLocalPortOkay(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
                  (get-local-port sock))`,
		Expect: "0",
	}).Test(t)
}

func TestSocketLocalPortOkay(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((sock (make-instance 'socket :socket ufd)))
                  (send sock :local-port))`,
		Expect: "0",
	}).Test(t)
}

func TestGetLocalPortHTTP(t *testing.T) {
	serv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("okay"))
	}))
	defer serv.Close()

	serv.Config.ConnState = func(nc net.Conn, cs http.ConnState) {
		if cs == http.StateActive {
			u, _ := url.Parse(serv.URL)
			scope := slip.NewScope()
			us := slip.ReadString("(make-instance 'socket)").Eval(scope, nil).(*flavors.Instance)
			tc, _ := nc.(*net.TCPConn)
			raw, _ := tc.SyscallConn()
			_ = raw.Control(func(fd uintptr) { us.Any = int(fd) })
			scope.Let(slip.Symbol("sock"), us)
			result := slip.ReadString("(send sock :local-port)").Eval(scope, nil).(slip.Fixnum)
			port, _ := strconv.Atoi(u.Port())
			tt.Equal(t, slip.Fixnum(port), result)
		}
	}
	if resp, err := serv.Client().Get(serv.URL); err == nil {
		_ = resp.Body.Close()
	}
}

func TestGetLocalPortNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(get-local-port t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketLocalPortClosed(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance 'socket) :local-port)`,
		Expect: "nil",
	}).Test(t)
}
