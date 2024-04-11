// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestServerActivep(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let ((ws (make-instance 'watch-server :port %d)))
 (send ws :activep))
`, port),
		Expect: "t",
	}).Test(t)
}

func TestServerConnect(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :eval (+ 2 3)))
`, port, port),
		Expect: "5",
	}).Test(t)
}

func TestServerShutdown(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send ws :shutdown)
 (do ((x (send ws :activep) (send ws :activep)))
     ((not x) x))
 (list (send ws :activep) (send ws :connections)))
`, port, port),
		Expect: "(nil ())",
	}).Test(t)
}

func TestServerConnections(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (send ws :connections))
`, port, port),
		Expect: `/\(\("127.0.0.1:[0-9]+" \(watching\) \(periodics\)\)\)/`,
	}).Test(t)
}

func TestServerConnectionsArgCount(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d)))
 (send ws :connections t))
`, port),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestServerShutdownArgCount(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d)))
 (send ws :shutdown t))
`, port),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestServerActivepArgCount(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d)))
 (send ws :activep t))
`, port),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestServerChange(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (chan (make-channel 3))
       (wc (make-instance 'watch-channeler :host "127.0.0.1" :port %d :channel chan :watch '(quux))))
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (defvar quux 5)
 (channel-pop chan))
`, port, port),
		Expect: "(quux 5)",
	}).Test(t)
}

func TestServerBadPort(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'watch-server :port t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source:    `(make-instance 'watch-server :port 100000)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestServerDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":init",
		":shutdown",
		":connections",
		":activep",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method watch-server %s out)`, method)).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}
