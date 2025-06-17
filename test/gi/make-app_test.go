// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeAppBasic(t *testing.T) {
	// Use a directory outside the slip repo to avoid the clash of the slip
	// go.mod and the application go.mod.
	scratchPath := filepath.Join(os.TempDir(), "scratch")
	if td := os.Getenv("RUNNER_TEMP"); 0 < len(td) {
		scratchPath = filepath.Join(td, "scratch")
		t.Skip("CI has issues when 'go mod tidy' is called.")
	}
	_ = os.RemoveAll(scratchPath)
	_ = os.RemoveAll("testdata/quux")
	_ = os.RemoveAll("testdata/make-app.out")
	defer func() { _ = os.RemoveAll(scratchPath) }()

	wd, _ := os.Getwd()
	(&sliptest.Function{
		Source: fmt.Sprintf(`(make-app "testdata/quux" '("testdata/quux.lisp") 'quux
                                       :options '((:flag "num" :doc "a number" :default 1 :type fixnum :var num))
                                       :key "secret"
                                       :key-flag "key"
                                       :key-file "testdata/.quux-key"
                                       :replace %q
                                       :cleanup nil
                                       :usage "just a test"
                                       :scratch %q)`, filepath.Dir(filepath.Dir(wd)), scratchPath),
		Expect: `/scratch"$/`,
	}).Test(t)
	cmd := exec.Command("testdata/quux", "-key", "secret", "-num", "3")
	err := cmd.Run()
	tt.Nil(t, err)

	var content []byte
	content, err = os.ReadFile("testdata/make-app.out")
	tt.Nil(t, err)
	tt.Equal(t, "make-app ran with num argument of 3", strings.TrimSpace(string(content)))
}

func TestMakeAppBadAppFilepath(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-app t '("testdata/quux.lisp") 'quux)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakeAppBadFiles(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-app "testdata/quux" '(t) 'quux)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-app "testdata/quux" t 'quux)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakeAppBadOptions(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-app "testdata/quux" '("testdata/quux.lisp") 'quux :options '(t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-app "testdata/quux" '("testdata/quux.lisp") 'quux :options t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-app "testdata/quux" '("testdata/quux.lisp") 'quux
                           :options '((:flag "num" :default 1 :type fixnum :var num :bad t)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakeAppPluginNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-app "testdata/quux" '("testdata/quux.lisp") 'quux :plugins '(not-found))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-app "testdata/quux" '("testdata/quux.lisp") 'quux :plugins '("~/not-found.so"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-app "testdata/quux" '("testdata/quux.lisp") 'quux :plugins t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakeAppPluginLoadPaths(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*package-load-path* "testdata"))
                      (make-app "testdata/quux" '("testdata/quux.lisp") 'quux :plugins '(not-found)))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*package-load-path* '("testdata" t)))
                      (make-app "testdata/quux" '("testdata/quux.lisp") 'quux :plugins '(not-found)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((*package-load-path* t))
                      (make-app "testdata/quux" '("testdata/quux.lisp") 'quux :plugins '(not-found)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
