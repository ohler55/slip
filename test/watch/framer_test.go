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

func TestFramerBorderLine(t *testing.T) {
	scope := slip.NewScope()
	var out safeWriter
	orig := scope.Get("*standard-output*")
	defer scope.Set("*standard-output*", orig)
	scope.Set("*standard-output*", &slip.OutputStream{Writer: &out})
	done := make(gi.Channel, 1)
	scope.Set("done", done)
	go func() {
		for out.Len() < 1000 { // target is 1062
			time.Sleep(time.Millisecond * 50)
		}
		done <- slip.True
	}()
	port := availablePort()
	(&sliptest.Function{
		Scope: scope,
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-framer :host "127.0.0.1" :port %d :top 2 :left 3 :border :line :watch '(foofoo))))
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (defvar foofoo 5)
 (channel-pop done)
 (send ws :shutdown))
`, port, port),
		Expect: "nil",
	}).Test(t)

	str := strings.ReplaceAll(out.String(), "\x1b", "")
	tt.Equal(t, "/foofoo: 5/", str)
	// Verify the border was drawn
	tt.Equal(t, "/┌──────/", str)
	tt.Equal(t, "/┕──────/", str)
}

func TestFramerBorderChar(t *testing.T) {
	scope := slip.NewScope()
	var out safeWriter
	orig := scope.Get("*standard-output*")
	defer scope.Set("*standard-output*", orig)
	scope.Set("*standard-output*", &slip.OutputStream{Writer: &out})
	done := make(gi.Channel, 1)
	scope.Set("done", done)
	go func() {
		for out.Len() < 400 { // target is 422
			time.Sleep(time.Millisecond * 50)
		}
		done <- slip.True
	}()
	port := availablePort()
	(&sliptest.Function{
		Scope: scope,
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-framer :host "127.0.0.1" :port %d :top 2 :left 3 :border #\# :watch '(foo2))))
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (defvar foo2 6)
 (channel-pop done)
 (send ws :shutdown))
`, port, port),
		Expect: "nil",
	}).Test(t)
	str := strings.ReplaceAll(out.String(), "\x1b", "")
	tt.Equal(t, "/foo2: 6/", str)
	// Verify the border was drawn
	tt.Equal(t, "/#####/", str)
}

func TestFramerNoBorder(t *testing.T) {
	scope := slip.NewScope()
	var out safeWriter
	orig := scope.Get("*standard-output*")
	defer scope.Set("*standard-output*", orig)
	scope.Set("*standard-output*", &slip.OutputStream{Writer: &out})
	done := make(gi.Channel, 3)
	scope.Set("done", done)
	go func() {
		for out.Len() < 120 { // target is 128
			time.Sleep(time.Millisecond * 50)
		}
		done <- slip.True
		for out.Len() < 220 { // target is 225
			time.Sleep(time.Millisecond * 50)
		}
		done <- slip.True
		for out.Len() < 300 { // target is 322
			time.Sleep(time.Millisecond * 50)
		}
		done <- slip.True
	}()
	port := availablePort()
	(&sliptest.Function{
		Scope: scope,
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-framer :host "127.0.0.1" :port %d :top -1 :left -1 :border nil :watch '(foo3))))
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (defvar foo3 (string-repeat "abcd" 500))
 (channel-pop done)
 (send wc :refresh)
 (channel-pop done)
 (send wc :forget 'foo4)
 (channel-pop done)
 (send ws :shutdown))
`, port, port),
		Expect: "nil",
	}).Test(t)
	str := strings.ReplaceAll(out.String(), "\x1b", "")
	tt.Equal(t, `/foo3: "abcd.*\.\.\./`, str)
}

func TestFramerErrorValue(t *testing.T) {
	scope := slip.NewScope()
	var out safeWriter
	orig := scope.Get("*standard-output*")
	defer scope.Set("*standard-output*", orig)
	scope.Set("*standard-output*", &slip.OutputStream{Writer: &out})
	done := make(gi.Channel, 3)
	scope.Set("done", done)
	go func() {
		for out.Len() < 80 { // target is 84
			time.Sleep(time.Millisecond * 50)
		}
		done <- slip.True
	}()
	port := availablePort()
	(&sliptest.Function{
		Scope: scope,
		Source: fmt.Sprintf(`
(let* ((ws (make-instance 'watch-server :port %d))
       (wc (make-instance 'watch-framer
                          :host "127.0.0.1"
                          :port %d
                          :top 1
                          :left 1
                          :border nil
                          :periodics '((p5 0.1 (lambda () (/ 1 0)))))))
 (do ((x (send ws :connections) (send ws :connections)))
     ((not (null x)) x))
 (channel-pop done)
 (send ws :shutdown))
`, port, port),
		Expect: "nil",
	}).Test(t)
	str := strings.ReplaceAll(out.String(), "\x1b", "")
	tt.Equal(t, `/p5: #<division-by-zero: divide by zero>/`, str)
}

func TestFramerDocs(t *testing.T) {
	scope := slip.NewScope()
	var out safeWriter
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":init",
		":changed",
		":forget",
		":refresh",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method watch-framer %s out)`, method), scope).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}
