// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch_test

import (
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestPrinterBorderLine(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	orig := scope.Get("*standard-output*")
	defer scope.Set("*standard-output*", orig)
	scope.Set("*standard-output*", &slip.OutputStream{Writer: &out})
	done := make(gi.Channel, 1)
	scope.Set("done", done)
	go func() {
		for out.Len() < 8 { // target is 10
			time.Sleep(time.Millisecond * 50)
		}
		done <- slip.True
	}()
	port := availablePort()
	(&sliptest.Function{
		Scope: scope,
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-printer :host "127.0.0.1" :port %d)))
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (defvar poobar 5)
 (send wc :watch 'poobar)
 (channel-pop done)
 (send wc :shutdown)
 (send ws :shutdown))
`, port, port),
		Expect: "nil",
	}).Test(t)

	str := out.String()
	tt.Equal(t, "poobar: 5\n", str)
}

func TestPrinterDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":changed",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method watch-printer %s out)`, method), scope).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}
