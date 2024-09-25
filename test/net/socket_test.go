// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"fmt"
	"os"
	"strings"
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketInitNil(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-instance 'socket)`,
		Expect: "/#<socket [0-9a-f]+>/",
	}
	tf.Test(t)
	inst, ok := tf.Result.(*flavors.Instance)
	tt.Equal(t, true, ok)
	tt.Nil(t, inst.Any)
}

func TestSocketFd(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
	}()
	scope := slip.NewScope()
	scope.Let("ufd", slip.Fixnum(fds[0]))
	scope.Let("sock", slip.Fixnum(fds[0]))
	tf := sliptest.Function{
		Scope:  scope,
		Source: `(setq sock (make-instance 'socket :socket ufd))`,
		Expect: "/#<socket [0-9a-f]+>/",
	}
	tf.Test(t)
	inst, ok := tf.Result.(*flavors.Instance)
	tt.Equal(t, true, ok)
	tt.SameType(t, 0, inst.Any) // int
	socket := slip.ReadString(`(send sock :socket)`).Eval(scope, nil)
	tt.SameType(t, slip.Fixnum(0), socket)
	state := slip.ReadString(`(send sock :state)`).Eval(scope, nil)
	tt.Equal(t, slip.Symbol(":write"), state)
}

func TestSocketFile(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	f0 := os.NewFile(uintptr(fds[0]), "file-0")
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
		_ = f0.Close()
	}()
	scope := slip.NewScope()
	scope.Let("ufile", (*slip.FileStream)(f0))
	tf := sliptest.Function{
		Scope:  scope,
		Source: `(make-instance 'socket :socket ufile)`,
		Expect: "/#<socket [0-9a-f]+>/",
	}
	tf.Test(t)
	inst, ok := tf.Result.(*flavors.Instance)
	tt.Equal(t, true, ok)
	tt.SameType(t, 0, inst.Any) // int
}

func TestSocketInitCreate(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-instance 'socket :domain :inet :type :stream :protocol :ip) :type)`,
		Expect: ":stream",
	}).Test(t)
}

func TestSocketInitBadCreate(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'socket :domain :unix :type :raw :protocol :udp)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSocketNotStream(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("uconn", &slip.IOStream{})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(make-instance 'socket :socket uconn)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketBadSocket(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'socket :socket t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":address",
		":close",
		":init",
		":make-stream",
		":name",
		":open-p",
		":option",
		":peer-address",
		":peer-name",
		":peer-port",
		":port",
		":receive",
		":send",
		":set-option",
		":set-socket",
		":socket",
		":state",
		":stream",
		":type",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method socket %s out)`, method)).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}
