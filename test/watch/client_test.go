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

func TestClientPeriodic(t *testing.T) {
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
                          :periodics '((puux 0.11 (lambda () pux))))))
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (setq result (channel-pop chan))
 (send wc :forget 'puux)
 result)
`, port, port),
		Expect: `(puux 3)`,
	}).Test(t)
}

func TestClientDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":init",
		":shutdown",
		":eval",
		":watch",
		":forget",
		":changed",
		":periodic",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method watch-client %s out)`, method)).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}
