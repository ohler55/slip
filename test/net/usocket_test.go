// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"fmt"
	"net"
	"os"
	"strings"
	"syscall"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestUsocketInitNil(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-instance 'usocket)`,
		Expect: "/#<usocket [0-9a-f]+>/",
	}
	tf.Test(t)
	inst, ok := tf.Result.(*flavors.Instance)
	tt.Equal(t, true, ok)
	tt.Nil(t, inst.Any)
}

func TestUsocketFd(t *testing.T) {
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
		Source: `(setq sock (make-instance 'usocket :socket ufd))`,
		Expect: "/#<usocket [0-9a-f]+>/",
	}
	tf.Test(t)
	inst, ok := tf.Result.(*flavors.Instance)
	tt.Equal(t, true, ok)
	tt.SameType(t, &net.UnixConn{}, inst.Any)
	socket := slip.ReadString(`(send sock :socket)`).Eval(scope, nil)
	tt.SameType(t, &slip.IOStream{}, socket)
	state := slip.ReadString(`(send sock :state)`).Eval(scope, nil)
	tt.Equal(t, slip.Symbol(":read-write"), state)
}

func TestUsocketFile(t *testing.T) {
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
		Source: `(make-instance 'usocket :socket ufile)`,
		Expect: "/#<usocket [0-9a-f]+>/",
	}
	tf.Test(t)
	inst, ok := tf.Result.(*flavors.Instance)
	tt.Equal(t, true, ok)
	tt.SameType(t, &net.UnixConn{}, inst.Any)
}

func TestUsocketConn(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	f0 := os.NewFile(uintptr(fds[0]), "file-0")
	fc, err := net.FileConn(f0)
	tt.Nil(t, err)
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
		_ = f0.Close()
		_ = fc.Close()
	}()
	scope := slip.NewScope()
	scope.Let("uconn", &slip.IOStream{RW: fc})
	tf := sliptest.Function{
		Scope:  scope,
		Source: `(make-instance 'usocket :socket uconn)`,
		Expect: "/#<usocket [0-9a-f]+>/",
	}
	tf.Test(t)
	inst, ok := tf.Result.(*flavors.Instance)
	tt.Equal(t, true, ok)
	tt.SameType(t, &net.UnixConn{}, inst.Any)
}

func TestUsocketDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":init",
		":socket",
		":set-socket",
		":state",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method usocket %s out)`, method)).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}
