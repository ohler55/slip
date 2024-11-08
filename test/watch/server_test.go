// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch_test

import (
	"fmt"
	"net"
	"strings"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
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
       (wc (make-instance 'watch-client
                          :host "127.0.0.1"
                          :port %d
                          :watch '(quux)
                          :periodics '((p5 3 qq)(p6 3 (lambda () (now)))))))
 (do ((x (send ws :connections) (send ws :connections)))
     ((< 1 (length (nth 2 (car  x)))) x))
 (send ws :connections))
`, port, port),
		Readably: true,
		Validate: func(t *testing.T, v slip.Object) {
			list := v.(slip.List)
			list = list[0].(slip.List)
			tt.Equal(t, "(watching quux)", slip.ObjectString(list[1]))
			list = list[2].(slip.List)
			for _, a := range list[1:] {
				switch a.(slip.List)[0].(slip.List).Cdr() {
				case slip.Symbol("p5"):
					tt.Equal(t, "((id . p5) (period . 3) (op . qq))", slip.ObjectString(a))
				case slip.Symbol("p6"):
					tt.Equal(t, "/\\(\\(id \\. p6\\) \\(period \\. 3\\) \\(op \\. .*\\(lambda \\(\\)/",
						slip.ObjectString(a))
				}
			}
		},
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

func TestServerProtocolOk(t *testing.T) {
	scope := slip.NewScope()
	port := availablePort()
	ws := slip.ReadString(
		fmt.Sprintf(`(make-instance 'watch-server :port %d)`, port)).Eval(scope, nil).(*flavors.Instance)
	defer func() { _ = ws.Receive(scope, ":shutdown", slip.List{}, 0) }()

	con, err := net.Dial("tcp", fmt.Sprintf("localhost:%d", port))
	tt.Nil(t, err)
	defer func() { _ = con.Close() }()

	reply := make([]byte, 1024)
	var cnt int

	// Make sure partial reads work.
	_, err = con.Write([]byte("(eval 3 (+ 2"))
	tt.Nil(t, err)
	// Need a little sleep to force a read completion on the server.
	time.Sleep(time.Millisecond * 10)
	_, err = con.Write([]byte(" 3))"))
	tt.Nil(t, err)

	cnt, err = con.Read(reply)
	tt.Nil(t, err)
	tt.Equal(t, "(result 3 5)", string(reply[:cnt]))

	_, err = con.Write([]byte("bad)"))
	tt.Nil(t, err)

	cnt, err = con.Read(reply)
	tt.Nil(t, err)
	tt.Equal(t, `(error nil parse-error "unmatched close parenthesis at 0:3")`, string(reply[:cnt]))
}
