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

func TestClientPeriodics(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(defvar pux 3)
(let* ((ws (make-instance 'watch-server :port %d))
       (result nil)
       (chan (make-channel 3))
       (wc (make-instance 'watch-channeler
                          :host "127.0.0.1"
                          :port %d
                          :channel chan
                          :periodics '((puux 0.05 (lambda () pux))))))
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (setq result (channel-pop chan))
 (send wc :forget 'puux)
 result)
`, port, port),
		Expect: `(puux 3)`,
	}).Test(t)
}

func TestClientPeriodicLambda(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(defvar pux 3)
(let* ((ws (make-instance 'watch-server :port %d))
       (result nil)
       (chan (make-channel 3))
       (wc (make-instance 'watch-channeler
                          :host "127.0.0.1"
                          :port %d
                          :channel chan)))
 (send wc :periodic 'puux 0.05 (lambda () pux))
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (setq result (channel-pop chan))
 (send wc :forget 'puux)
 result)
`, port, port),
		Expect: `(puux 3)`,
	}).Test(t)
}

func TestClientPeriodicSymbol(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(defvar puv 3)
(let* ((ws (make-instance 'watch-server :port %d))
       (chan (make-channel 3))
       (wc (make-instance 'watch-channeler
                          :host "127.0.0.1"
                          :port %d
                          :channel chan)))
 (send wc :periodic 'puuv 0.05 'pux)
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (channel-pop chan))
`, port, port),
		Expect: `(puuv 3)`,
	}).Test(t)
}

func TestClientPeriodicBadOp(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (chan (make-channel 3))
       (wc (make-instance 'watch-channeler
                          :host "127.0.0.1"
                          :port %d
                          :channel chan)))
 (send wc :periodic 'pub 0.05 7)
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (channel-pop chan))
`, port, port),
		Expect: `/\(pub #<ERROR [0-9a-f]+>\)/`,
	}).Test(t)
}

func TestClientInitBadHost(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'watch-client :host t :port 3333)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestClientInitBadPort(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'watch-client :host "localhost" :port t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestClientInitBadWatch(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'watch-client :host "localhost" :port 3333 :watch t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance 'watch-client :host "localhost" :port 3333 :watch '(t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestClientInitDialFail(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'watch-client :host "localhost" :port 100000)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestClientWriteFail(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (do ((x (send wc :activep) (send wc :activep)))
     ((not (null x)) x))
 (send wc :shutdown)
 (do ((x (send wc :activep) (send wc :activep)))
     ((not x) x))
 (send wc :watch 'foo))
`, port, port),
		Expect: "/#<ERROR [0-9a-f]+>/",
	}).Test(t)
}

func TestClientBadPeriodics(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d :periodics t))))
`, port, port),
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestClientBadPeriodics2(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d :periodics '(t)))))
`, port, port),
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestClientShutdownArgCount(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :shutdown t))
`, port, port),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestClientActivepArgCount(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :activep t))
`, port, port),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestClientWatchArgCount(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :watch))
`, port, port),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestClientForgetArgCount(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :forget))
`, port, port),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestClientChangedArgCount(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :changed 'foo))
`, port, port),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestClientPeriodicArgCount(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :periodic 'foo 1))
`, port, port),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestClientPeriodicBadSymbol(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :periodic t 1 'foo))
`, port, port),
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestClientEvalOk(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :eval '(+ 3 4)))
`, port, port),
		Expect: `7`,
	}).Test(t)
}

func TestClientEvalError(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :eval '(/ 1 0)))
`, port, port),
		Expect: "/#<DIVISION-BY-ZERO [0-9a-f]+>/",
	}).Test(t)
}

func TestClientEvalTimeout(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :eval '(sleep 0.1) :timeout 0.05))
`, port, port),
		Expect: `/#<ERROR [0-9a-f]+>/`,
	}).Test(t)
}

func TestClientEvalBadTimeout(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :eval '(+ 1 2) :timeout t))
`, port, port),
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestClientEvalArgCount(t *testing.T) {
	port := availablePort()
	(&sliptest.Function{
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-client :host "127.0.0.1" :port %d)))
 (send wc :eval))
`, port, port),
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestClientDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":init",
		":shutdown",
		":activep",
		":eval",
		":watch",
		":forget",
		":changed",
		":periodic",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method watch-client %s out)`, method), scope).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}
