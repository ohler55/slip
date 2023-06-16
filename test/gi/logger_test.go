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
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor :out out))").Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(progn (send logger :error "error ~A" 1) (send logger :shutdown))`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "E error 1\n", log.String())
	tt.Equal(t, "#<flavor logger-flavor>", gi.Logger().String())
	result := slip.ReadString("(send logger :out)").Eval(scope, nil)
	tt.Equal(t, "#<OUTPUT-STREAM>", slip.ObjectString(result))
}

func TestLoggerTimeLevelColor(t *testing.T) {
	var log bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &log})
	_ = slip.ReadString(
		"(setq logger (make-instance 'logger-flavor :out out :with-time t :with-level t :colorize t))",
	).Eval(scope, nil)

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
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor :json t))").Eval(scope, nil)
	_ = slip.ReadString("(send logger :set-level 5)").Eval(scope, nil)
	_ = slip.ReadString("(send logger :set-out log-out)").Eval(scope, nil)

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
		":out",
		":set-out",
		":write",
		":shutdown",
	} {
		out.Reset()
		_ = slip.ReadString(fmt.Sprintf(`(describe-method 'logger-flavor %s out)`, method)).Eval(scope, nil)
		str := out.String()
		tt.Equal(t, true, strings.Contains(str, "logger-flavor"))
		tt.Equal(t, true, strings.Contains(str, method))
	}
}

func TestLoggerNilOut(t *testing.T) {
	var log bytes.Buffer
	scope := slip.NewScope()
	scope.Set(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &log})
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor :out nil))").Eval(scope, nil)
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
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor))").Eval(scope, nil)
	_ = slip.ReadString("(send logger :set-out nil)").Eval(scope, nil)
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
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor))").Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send logger :set-out)`,
		Panics: true,
	}).Test(t)
}

func TestLoggerSetOutBad(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq logger (make-instance 'logger-flavor))").Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send logger :set-out t)`,
		Panics: true,
	}).Test(t)
}
