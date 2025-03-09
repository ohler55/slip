// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"bytes"
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestLoggerDefault(t *testing.T) {
	var log bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &log})
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor :out out))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(progn (send logger :error "error ~A" 1) (send logger :shutdown))`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "E error 1\n", log.String())
	tt.Equal(t, "#<flavor logger-flavor>", gi.Logger().String())
	result := slip.ReadString("(send logger :out)", scope).Eval(scope, nil)
	tt.Equal(t, "#<OUTPUT-STREAM>", slip.ObjectString(result))
}

func TestLoggerTimeLevelColor(t *testing.T) {
	var log bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &log})
	_ = slip.ReadString(
		"(setq logger (make-instance 'logger-flavor :out out :with-time t :with-level t :colorize t))",
		scope).Eval(scope, nil)

	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (send logger :warn "warning ~A" 2)
                  (send logger :info "info ~A" 3)
                  (send logger :shutdown))`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "/^.*T.*Z \x1b\\[33mW warning 2\x1b\\[m\n/", log.String())
}

func TestLoggerJSON(t *testing.T) {
	var log bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("log-out"), &slip.OutputStream{Writer: &log})
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor :json t))", scope).Eval(scope, nil)
	_ = slip.ReadString("(send logger :set-level 5)", scope).Eval(scope, nil)
	_ = slip.ReadString("(send logger :set-out log-out)", scope).Eval(scope, nil)

	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (send logger :info "info ~A" 3)
                  (send logger :debug "debug ~A" 4)
                  (send logger :shutdown))`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `/^\{"level":"info","message":"info 3","when":".*T.*Z"\}
\{"level":"debug","message":"debug 4","when":".*T.*Z"\}
/`, log.String())
}

func TestLoggerLog(t *testing.T) {
	var log bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("log-out"), &slip.OutputStream{Writer: &log})
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor))", scope).Eval(scope, nil)
	_ = slip.ReadString("(send logger :set-out log-out)", scope).Eval(scope, nil)

	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (send logger :log :error "error ~A" 0)
                  (send logger :log :warn "warn ~A" 1)
                  (send logger :log :info "info ~A" 2)
                  (send logger :log :debug "debug ~A" 3)
                  (send logger :log 2 "info ~A" 4)
                  (send logger :log -1 "low ~A" 5)
                  (send logger :log 4 "high ~A" 6)
                  (send logger :shutdown))`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `E error 0
W warn 1
I info 2
D debug 3
I info 4
E low 5
D high 6
`, log.String())
}

func TestLoggerDescribe(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":init",
		":error",
		":warn",
		":info",
		":debug",
		":log",
		":out",
		":set-out",
		":write",
		":shutdown",
	} {
		out.Reset()
		_ = slip.ReadString(fmt.Sprintf(`(describe-method 'logger-flavor %s out)`, method), scope).Eval(scope, nil)
		str := out.String()
		tt.Equal(t, true, strings.Contains(str, "logger-flavor"))
		tt.Equal(t, true, strings.Contains(str, method))
	}
}

func TestLoggerNilOut(t *testing.T) {
	var log bytes.Buffer
	scope := slip.NewScope()
	scope.Set(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &log})
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor :out nil))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(progn (send logger :error "standard error") (send logger :shutdown))`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "E standard error\n", log.String())
}

func TestLoggerSetOutNil(t *testing.T) {
	var log bytes.Buffer
	scope := slip.NewScope()
	scope.Set(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &log})
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor))", scope).Eval(scope, nil)
	_ = slip.ReadString("(send logger :set-out nil)", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(progn (send logger :error "standard error") (send logger :shutdown))`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "E standard error\n", log.String())
}

func TestLoggerBadOut(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-instance 'logger-flavor :out t)`,
		Panics: true,
	}).Test(t)
}

func TestLoggerSetOutArgCount(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send logger :set-out)`,
		Panics: true,
	}).Test(t)
}

func TestLoggerSetOutBad(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send logger :set-out t)`,
		Panics: true,
	}).Test(t)
}
