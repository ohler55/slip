// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"net"
	"net/http"
	"net/http/httptest"
	"net/url"
	"strconv"
	"strings"
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketNameOkay(t *testing.T) {
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
                  (socket-name sock))`,
		Expect: `"@", 0`,
	}).Test(t)
}

func TestSocketLocalNameOkay(t *testing.T) {
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
                  (send sock :name))`,
		Expect: `"@", 0`,
	}).Test(t)
}

func TestSocketLocalNameIPv6(t *testing.T) {
	nl, _ := net.Listen("tcp6", "[::]:7779")
	defer nl.Close()
	go func() {
		_, _ = nl.Accept()
	}()
	nc, _ := net.Dial("tcp6", "[::]:7779")
	scope := slip.NewScope()
	us := slip.ReadString("(make-instance 'socket)", scope).Eval(scope, nil).(*flavors.Instance)
	tc, _ := nc.(*net.TCPConn)
	raw, _ := tc.SyscallConn()
	_ = raw.Control(func(fd uintptr) { us.Any = int(fd) })
	scope.Let(slip.Symbol("sock"), us)
	result := slip.ReadString("(send sock :name)", scope).Eval(scope, nil).(slip.Values)

	addr := nc.LocalAddr().String()
	// url.Parse does not handle IPv6 host names so do it the hard way.
	pos := strings.LastIndexByte(addr, ':')
	port, _ := strconv.Atoi(addr[pos+1:])
	tt.Equal(t, slip.Fixnum(port), result[1])
	tt.Equal(t, slip.Octets{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1}, result[0])
}

func TestSocketNameHTTP(t *testing.T) {
	serv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("okay"))
	}))
	defer serv.Close()

	serv.Config.ConnState = func(nc net.Conn, cs http.ConnState) {
		if cs == http.StateActive {
			u, _ := url.Parse(serv.URL)
			scope := slip.NewScope()
			us := slip.ReadString("(make-instance 'socket)", scope).Eval(scope, nil).(*flavors.Instance)
			tc, _ := nc.(*net.TCPConn)
			raw, _ := tc.SyscallConn()
			_ = raw.Control(func(fd uintptr) { us.Any = int(fd) })
			scope.Let(slip.Symbol("sock"), us)
			result := slip.ReadString("(send sock :name)", scope).Eval(scope, nil).(slip.Values)
			port, _ := strconv.Atoi(u.Port())
			tt.Equal(t, slip.Fixnum(port), result[1])
			tt.Equal(t, slip.Octets{127, 0, 0, 1}, result[0])
		}
	}
	if resp, err := serv.Client().Get(serv.URL); err == nil {
		_ = resp.Body.Close()
	}
}

func TestSocketNameNotSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-name t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketLocalNameClosed(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance 'socket) :name)`,
		Expect: "nil, nil",
	}).Test(t)
}

func TestSocketLocalNameBadFd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(send (make-instance 'socket :socket 777) :name)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
