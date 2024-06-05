// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"fmt"
	"os"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSystemFetchFile(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :components '("testdata/comp.lisp")
                      :depends-on '((quux :file "testdata" "sys-test")))))
  (send sys :fetch)
  (send sys :load))
`,
		Expect: `nil`,
	}).Test(t)
	_, err := os.Stat("testout/quux/sys-test.lisp")
	tt.Nil(t, err)
	result := slip.ReadString("(sys-test-comp)").Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(3), result)

	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((quux :file "testdata" "nothing")))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSystemFetchGitTag(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((sst :git
                                         "https://github.com/ohler55/slip-system-test"
                                         :tag "v1.0.0"
                                         :scratch ".scratch"
                                         :sub-dir "lisp")))))
  (send sys :fetch))
`,
		Expect: `nil`,
	}).Test(t)
	_, err := os.Stat("testout/sst/sample.lisp")
	tt.Nil(t, err)
	var content []byte
	content, err = os.ReadFile("testout/sst/sample.lisp")
	tt.Nil(t, err)
	tt.Equal(t, `
(defun sample (x)
  (+ x 5))
`, string(content))

	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((sst :git
                                         "https://github.com/ohler55/slip-system-test"
                                         :tag "xxxx"
                                         :scratch ".scratch")))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache nil
                      :depends-on '((sst :git
                                         "https://github.com/ohler55/xxxxx"
                                         :tag "xxxx"
                                         :scratch ".scratch")))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSystemFetchGitBranch(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((sst :git
                                         "https://github.com/ohler55/slip-system-test"
                                         :branch "develop"
                                         :scratch ".scratch"
                                         :sub-dir "lisp")))))
  (send sys :fetch))
`,
		Expect: `nil`,
	}).Test(t)
	_, err := os.Stat("testout/sst/sample.lisp")
	tt.Nil(t, err)
	var content []byte
	content, err = os.ReadFile("testout/sst/sample.lisp")
	tt.Nil(t, err)
	tt.Equal(t, `
(defun sample (x)
  (+ x 8))

(defun quux ()
  'foobar)
`, string(content))

	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((sst :git
                                         "https://github.com/ohler55/slip-system-test"
                                         :branch "xxxx"
                                         :scratch ".scratch")))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((sst :git
                                         "https://github.com/ohler55/xxxxx"
                                         :branch "xxxx"
                                         :scratch ".scratch")))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSystemFetchGitCommit(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((sst :git
                                         "https://github.com/ohler55/slip-system-test"
                                         :commit "33e624d044" ;; 33e624d04465499b5f19980501bc5fef7e205ba1
                                         :scratch ".scratch"
                                         :sub-dir "lisp")))))
  (send sys :fetch))
`,
		Expect: `nil`,
	}).Test(t)
	_, err := os.Stat("testout/sst/sample.lisp")
	tt.Nil(t, err)
	var content []byte
	content, err = os.ReadFile("testout/sst/sample.lisp")
	tt.Nil(t, err)
	tt.Equal(t, `
(defun sample (x)
  (+ x 7))

(defun quux ()
  'foobar)
`, string(content))

	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((sst :git
                                         "https://github.com/ohler55/xxxxx"
                                         :commit "xxxx"
                                         :scratch ".scratch")))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((sst :git
                                         "https://github.com/ohler55/slip-system-test"
                                         :commit "xxxx"
                                         :scratch ".scratch")))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSystemFetchGitUnknown(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((sst :git
                                         "https://github.com/ohler55/slip-system-test")))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSystemFetchCall(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :author "peter@ohler.com"
                      :cache "testout"
                      :depends-on '((quux :call
                                          (with-open-file
                                           (f (join "/" cache-dir "author.lisp")
                                            :direction :output
                                            :if-does-not-exist :create
                                            :if-exists :supersede)
                                           (format f "(setq author ~S)~%" author)))))))
  (send sys :fetch))
`,
		Expect: `nil`,
	}).Test(t)
	_, err := os.Stat("testout/quux/author.lisp")
	tt.Nil(t, err)
	var content []byte
	content, err = os.ReadFile("testout/quux/author.lisp")
	tt.Nil(t, err)
	tt.Equal(t, `(setq author "peter@ohler.com")
`, string(content))

	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((quux :call t)))))
  (send sys :fetch))
`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSystemDescribe(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":fetch",
		":load",
		":run",
	} {
		out.Reset()
		_ = slip.ReadString(fmt.Sprintf(`(describe-method 'system %s out)`, method)).Eval(scope, nil)
		str := out.String()
		tt.Equal(t, true, strings.Contains(str, "system"))
		tt.Equal(t, true, strings.Contains(str, method))
	}
}

func TestSystemFetchBadDepends(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on t)))
  (send sys :fetch))
`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '(t))))
  (send sys :fetch))
`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSystemFetchBadCache(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache t
                      :depends-on '((quux :file "testdata" "sys-test")))))
  (send sys :fetch))
`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSystemFetchMkdirFail(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "Makefile"
                      :depends-on '((quux :file "testdata" "sys-test")))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSystemFetchMkdirFail2(t *testing.T) {
	_ = os.MkdirAll("testout", 0755)
	_ = os.WriteFile("testout/blocker", []byte("test"), 0666)
	defer func() { _ = os.RemoveAll("testout/blocker") }()

	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((blocker :file "testdata" "sys-test")))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSystemFetchCpFail(t *testing.T) {
	_ = os.WriteFile("testdata/no-read.lisp", []byte("test"), 0333)
	defer func() { _ = os.RemoveAll("testdata/no-read.lisp") }()

	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((quux :file "testdata" "no-read")))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSystemFetchUnknown(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let ((sys
       (make-instance 'system
                      :cache "testout"
                      :depends-on '((quux :bad t)))))
  (send sys :fetch))
`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
