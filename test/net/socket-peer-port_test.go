// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"net"
	"net/http"
	"net/http/httptest"
	"strconv"
	"strings"
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketPeerPortOkay(t *testing.T) {
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
                  (socket-peer-port sock))`,
		Expect: "0",
	}).Test(t)
}

func TestSocketSendPeerPortOkay(t *testing.T) {
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
                  (send sock :peer-port))`,
		Expect: "0",
	}).Test(t)
}

func TestSocketPeerPortHTTP(t *testing.T) {
	serv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("okay"))
	}))
	defer serv.Close()

	serv.Config.ConnState = func(nc net.Conn, cs http.ConnState) {
		if cs == http.StateActive {
			addr := nc.RemoteAddr().String()
			pos := strings.LastIndexByte(addr, ':')
			port, _ := strconv.Atoi(addr[pos+1:])

			scope := slip.NewScope()
			us := slip.ReadString("(make-instance 'socket)", scope).Eval(scope, nil).(*flavors.Instance)
			tc, _ := nc.(*net.TCPConn)
			raw, _ := tc.SyscallConn()
			_ = raw.Control(func(fd uintptr) { us.Any = int(fd) })
			scope.Let(slip.Symbol("sock"), us)
			result := slip.ReadString("(send sock :peer-port)", scope).Eval(scope, nil).(slip.Fixnum)
			tt.Equal(t, slip.Fixnum(port), result)
		}
	}
	if resp, err := serv.Client().Get(serv.URL); err == nil {
		_ = resp.Body.Close()
	}
}

func TestSocketPeerPortNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-peer-port t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketPeerPortClosed(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance 'socket) :peer-port)`,
		Expect: "nil",
	}).Test(t)
}
