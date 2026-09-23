// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"io"
	"os"
	"regexp"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
	"github.com/ohler55/slip/sliptest"
)

// *repl-key-bindings* variable.

func TestReplKeyBindingsDefaultNil(t *testing.T) {
	withKeyBindings(t, "")
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
}

func TestReplKeyBindingsSetGet(t *testing.T) {
	withKeyBindings(t, "")
	(&sliptest.Function{
		Source: `(progn
                   (setq *repl-key-bindings* '(("C-l" . line-end) ("M-x" . line-begin) ("C-g")))
                   *repl-key-bindings*)`,
		Expect: `(("C-l" . line-end) ("M-x" . line-begin) ("C-g"))`,
	}).Test(t)
	tt.Equal(t, "line-end", keyEval(t, `(repl-key-binding "C-l")`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "M-x")`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "C-g")`))
}

func TestReplKeyBindingsSetReplacesAll(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	_ = keyEval(t, `(setq *repl-key-bindings* '(("M-x" . line-begin)))`)
	tt.Equal(t, `(("M-x" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "C-l")`))
}

func TestReplKeyBindingsNilClears(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "C-l" 'line-end) (repl-bind-key "C-a" nil))`)
	_ = keyEval(t, `(setq *repl-key-bindings* nil)`)
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "C-l")`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "C-a")`))
}

func TestReplKeyBindingsNilActionDisables(t *testing.T) {
	withKeyBindings(t, `(setq *repl-key-bindings* '(("C-a" . nil)))`)
	tt.Equal(t, `(("C-a"))`, keyEval(t, `*repl-key-bindings*`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "C-a")`))
}

func TestReplKeyBindingsOrderPreserved(t *testing.T) {
	withKeyBindings(t, `(setq *repl-key-bindings* '(("M-y" . back-word) ("C-l" . line-end) ("M-x" . line-begin)))`)
	tt.Equal(t, `(("M-y" . back-word) ("C-l" . line-end) ("M-x" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
	_ = keyEval(t, `(repl-bind-key "C-g" 'forward-word)`)
	tt.Equal(t, `(("M-y" . back-word) ("C-l" . line-end) ("M-x" . line-begin) ("C-g" . forward-word))`,
		keyEval(t, `*repl-key-bindings*`))
}

func TestReplKeyBindingsReplaceEntryAfterSet(t *testing.T) {
	withKeyBindings(t, `(setq *repl-key-bindings* '(("C-l" . line-end) ("M-x" . line-begin)))`)
	_ = keyEval(t, `(repl-bind-key "C-l" 'back-word)`)
	tt.Equal(t, `(("C-l" . back-word) ("M-x" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
}

func TestReplKeyBindingsGetIsCopy(t *testing.T) {
	// Modifying the returned list must not change the bindings.
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	_ = keyEval(t, `(let ((b *repl-key-bindings*)) (setf (cdr (car b)) 'line-begin))`)
	tt.Equal(t, "line-end", keyEval(t, `(repl-key-binding "C-l")`))
}

func TestReplKeyBindingsReadBackCanonical(t *testing.T) {
	// Values set with alternate spellings still select the same keys.
	withKeyBindings(t, `(setq *repl-key-bindings* '(("C-i" . line-end) ("C-[x" . line-begin)))`)
	tt.Equal(t, "line-end", keyEval(t, `(repl-key-binding "TAB")`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "M-x")`))
}

// All or nothing: one bad entry leaves the previous bindings in place.
func TestReplKeyBindingsAllOrNothing(t *testing.T) {
	for _, x := range []struct {
		name  string
		value string
		msg   string
	}{
		{name: "bad key", value: `'(("M-x" . line-begin) ("C-" . line-end))`, msg: "/^invalid key/"},
		{name: "unknown action", value: `'(("M-x" . line-begin) ("C-g" . no-such-action))`, msg: "/unknown action/"},
		{name: "prefix clash", value: `'(("M-x" . line-begin) ("M-[1;5" . line-end))`, msg: "/^key .*prefix/"},
		{name: "not a cons", value: `'(("M-x" . line-begin) "C-g")`},
		{name: "key not a string", value: `'(("M-x" . line-begin) (7 . line-end))`},
		{name: "action not a symbol", value: `'(("M-x" . line-begin) ("C-g" . 7))`},
		{name: "bad first entry", value: `'(("" . line-begin) ("M-x" . line-end))`, msg: "/^invalid key/"},
	} {
		t.Run(x.name, func(t *testing.T) {
			withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
			p := keyError(t, `(setq *repl-key-bindings* `+x.value+`)`)
			if 0 < len(x.msg) {
				tt.Equal(t, x.msg, p.Error())
			}
			tt.Equal(t, `(("C-l" . line-end))`, keyEval(t, `*repl-key-bindings*`))
			tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-x")`))
		})
	}
}

func TestReplKeyBindingsNotList(t *testing.T) {
	for _, value := range []string{`7`, `"C-l"`, `'line-end`, `t`} {
		t.Run(value, func(t *testing.T) {
			withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
			(&sliptest.Function{
				Source:    `(setq *repl-key-bindings* ` + value + `)`,
				PanicType: slip.TypeErrorSymbol,
			}).Test(t)
			tt.Equal(t, `(("C-l" . line-end))`, keyEval(t, `*repl-key-bindings*`))
		})
	}
}

// Persistence through the REPL config file.

const keyConfigFile = "config/config.lisp"

func keyConfigSetup(t *testing.T) {
	t.Helper()
	tt.Nil(t, os.MkdirAll("config", 0755))
	withKeyBindings(t, "")
	tt.Nil(t, os.RemoveAll(keyConfigFile))
	repl.SetConfigDir("config")
	t.Cleanup(func() {
		resetKeyBindings(t)
		_ = os.RemoveAll(keyConfigFile)
	})
}

func readKeyConfig(t *testing.T) string {
	t.Helper()
	buf, err := os.ReadFile(keyConfigFile)
	tt.Nil(t, err)
	return string(buf)
}

// keyBindingsLine returns the setq line for *repl-key-bindings* from the
// config file.
func keyBindingsLine(t *testing.T, config string) string {
	t.Helper()
	for _, line := range strings.Split(config, "\n") {
		if strings.HasPrefix(line, "(setq *repl-key-bindings* ") {
			return line
		}
	}
	t.Fatalf("no *repl-key-bindings* setq in config:\n%s", config)
	return ""
}

func TestReplKeyBindingsPersistBindKey(t *testing.T) {
	keyConfigSetup(t)
	_ = keyEval(t, `(repl-bind-key "C-l" 'line-end)`)
	tt.Equal(t, `(setq *repl-key-bindings* '(("C-l" . line-end)))`, keyBindingsLine(t, readKeyConfig(t)))
}

func TestReplKeyBindingsPersistNilAction(t *testing.T) {
	keyConfigSetup(t)
	_ = keyEval(t, `(repl-bind-key "C-a" nil)`)
	tt.Equal(t, `(setq *repl-key-bindings* '(("C-a")))`, keyBindingsLine(t, readKeyConfig(t)))
}

func TestReplKeyBindingsPersistUnbindKey(t *testing.T) {
	keyConfigSetup(t)
	_ = keyEval(t, `(progn (repl-bind-key "C-l" 'line-end) (repl-bind-key "M-x" 'line-begin))`)
	tt.Nil(t, os.RemoveAll(keyConfigFile))
	_ = keyEval(t, `(repl-unbind-key "C-l")`)
	tt.Equal(t, `(setq *repl-key-bindings* '(("M-x" . line-begin)))`, keyBindingsLine(t, readKeyConfig(t)))
}

func TestReplKeyBindingsPersistSetq(t *testing.T) {
	keyConfigSetup(t)
	_ = keyEval(t, `(setq *repl-key-bindings* '(("M-x" . line-begin)))`)
	tt.Equal(t, `(setq *repl-key-bindings* '(("M-x" . line-begin)))`, keyBindingsLine(t, readKeyConfig(t)))
}

func TestReplKeyBindingsPersistRoundTrip(t *testing.T) {
	keyConfigSetup(t)
	_ = keyEval(t, `(progn
                      (repl-bind-key "C-l" 'line-end)
                      (repl-bind-key "M-[1;5C" 'back-word)
                      (repl-bind-key "C-\\" 'forward-word)
                      (repl-bind-key "M-C-b" 'line-begin)
                      (repl-bind-key "C-a" nil))`)
	before := keyEval(t, `*repl-key-bindings*`)
	config := readKeyConfig(t)
	keyBindingsLine(t, config) // must be present

	// Clear, put the saved file back, and load it the way the REPL does.
	_ = keyEval(t, `(setq *repl-key-bindings* nil)`)
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
	tt.Nil(t, os.WriteFile(keyConfigFile, []byte(config), 0666))
	repl.SetConfigDir("config")

	tt.Equal(t, before, keyEval(t, `*repl-key-bindings*`))
	tt.Equal(t, "forward-word", keyEval(t, `(repl-key-binding "C-\\")`))
	tt.Equal(t, "back-word", keyEval(t, `(repl-key-binding "M-[1;5C")`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "C-a")`))
}

func TestReplKeyBindingsPersistCleared(t *testing.T) {
	keyConfigSetup(t)
	_ = keyEval(t, `(repl-bind-key "C-l" 'line-end)`)
	_ = keyEval(t, `(setq *repl-key-bindings* nil)`)
	tt.Equal(t, `(setq *repl-key-bindings* nil)`, keyBindingsLine(t, readKeyConfig(t)))
}

// captureStdout runs f with os.Stdout redirected and returns what was written.
func captureStdout(t *testing.T, f func()) string {
	t.Helper()
	r, w, err := os.Pipe()
	tt.Nil(t, err)
	orig := os.Stdout
	os.Stdout = w
	done := make(chan string)
	go func() {
		buf, _ := io.ReadAll(r)
		done <- string(buf)
	}()
	defer func() {
		os.Stdout = orig
	}()
	f()
	_ = w.Close()
	os.Stdout = orig
	return <-done
}

// Addendum A item 2: a bad entry in the config file must not stop the REPL
// from starting. The bad entry is dropped with a warning naming it, the rest
// of the bindings apply and later lines of the config still load.
func TestReplKeyBindingsConfigLoadDropsBadEntries(t *testing.T) {
	for _, x := range []struct {
		name   string
		entry  string
		named  string
		reason string
	}{
		{name: "unknown action", entry: `("C-g" . no-such-action)`, named: "C-g", reason: "unknown action"},
		{name: "bad key", entry: `("C-" . line-end)`, named: "C-", reason: "invalid key"},
		{name: "prefix clash", entry: `("M-[1;5" . line-end)`, named: "M-[1;5", reason: "prefix"},
		{name: "chord", entry: `("M-M-x" . line-end)`, named: "M-M-x", reason: "chord"},
		{name: "not a cons", entry: `"C-g"`, named: "C-g"},
	} {
		t.Run(x.name, func(t *testing.T) {
			keyConfigSetup(t)
			scope := repl.GetScope()
			origPrompt := scope.Get(slip.Symbol("*repl-prompt*"))
			origOut := scope.Get(slip.Symbol("*standard-output*"))
			t.Cleanup(func() {
				scope.Set(slip.Symbol("*standard-output*"), origOut)
				scope.Set(slip.Symbol("*repl-prompt*"), origPrompt)
			})
			var out strings.Builder
			scope.Set(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})

			config := `;;;; test config

(setq *repl-key-bindings* '(("C-l" . line-end) ` + x.entry + ` ("M-x" . line-begin)))
(setq *repl-prompt* "zz> ")
`
			tt.Nil(t, os.WriteFile(keyConfigFile, []byte(config), 0666))
			stdout := captureStdout(t, func() {
				defer func() {
					if r := recover(); r != nil {
						t.Errorf("loading the config failed: %v", r)
					}
				}()
				repl.SetConfigDir("config")
			})

			tt.Equal(t, `(("C-l" . line-end) ("M-x" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
			tt.Equal(t, slip.String("zz> "), scope.Get(slip.Symbol("*repl-prompt*")))
			warning := out.String() + stdout
			tt.Equal(t, true, strings.Contains(warning, x.named), "warning %q does not name %s", warning, x.named)
			tt.Equal(t, true, strings.Contains(warning, x.reason), "warning %q does not give the reason %q", warning, x.reason)
		})
	}
}

// A non-list value in the config file is dropped with a warning; the rest
// of the file still loads and no bindings are set.
func TestReplKeyBindingsConfigLoadNotList(t *testing.T) {
	keyConfigSetup(t)
	_ = keyEval(t, `(repl-bind-key "C-l" 'line-end)`)
	scope := repl.GetScope()
	origPrompt := scope.Get(slip.Symbol("*repl-prompt*"))
	origOut := scope.Get(slip.Symbol("*standard-output*"))
	t.Cleanup(func() {
		scope.Set(slip.Symbol("*standard-output*"), origOut)
		scope.Set(slip.Symbol("*repl-prompt*"), origPrompt)
	})
	var out strings.Builder
	scope.Set(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	config := `;;;; test config

(setq *repl-key-bindings* 7)
(setq *repl-prompt* "zz> ")
`
	tt.Nil(t, os.WriteFile(keyConfigFile, []byte(config), 0666))
	stdout := captureStdout(t, func() {
		defer func() {
			if r := recover(); r != nil {
				t.Errorf("loading the config failed: %v", r)
			}
		}()
		repl.SetConfigDir("config")
	})
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
	tt.Equal(t, slip.String("zz> "), scope.Get(slip.Symbol("*repl-prompt*")))
	warning := out.String() + stdout
	tt.Equal(t, true, strings.Contains(warning, "*repl-key-bindings*"), "no warning in %q", warning)
}

// With an ANSI sequence in *repl-warning-prefix* the config-load warning is
// wrapped in the prefix and the ANSI reset.
func TestReplKeyBindingsConfigLoadWarningANSI(t *testing.T) {
	keyConfigSetup(t)
	scope := repl.GetScope()
	origWarn := scope.Get(slip.Symbol("*repl-warning-prefix*"))
	origOut := scope.Get(slip.Symbol("*standard-output*"))
	t.Cleanup(func() {
		scope.Set(slip.Symbol("*standard-output*"), origOut)
		scope.Set(slip.Symbol("*repl-warning-prefix*"), origWarn)
	})
	scope.Set(slip.Symbol("*repl-warning-prefix*"), slip.String("\x1b[33mWARN "))
	var out strings.Builder
	scope.Set(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	config := `;;;; test config

(setq *repl-key-bindings* '(("C-l" . line-end) ("C-g" . no-such-action)))
`
	tt.Nil(t, os.WriteFile(keyConfigFile, []byte(config), 0666))
	stdout := captureStdout(t, func() {
		defer func() {
			if r := recover(); r != nil {
				t.Errorf("loading the config failed: %v", r)
			}
		}()
		repl.SetConfigDir("config")
	})
	warning := out.String() + stdout
	tt.Equal(t, true,
		regexp.MustCompile("\x1b\\[33mWARN warning: dropped .*C-g.*unknown action.*\x1b\\[m\n").MatchString(warning),
		"warning %q", warning)
	tt.Equal(t, `(("C-l" . line-end))`, keyEval(t, `*repl-key-bindings*`))
}

func TestReplKeyBindingsConfigLoadWarningPlain(t *testing.T) {
	// Without ANSI in the prefix there is no reset suffix.
	keyConfigSetup(t)
	scope := repl.GetScope()
	origWarn := scope.Get(slip.Symbol("*repl-warning-prefix*"))
	origOut := scope.Get(slip.Symbol("*standard-output*"))
	t.Cleanup(func() {
		scope.Set(slip.Symbol("*standard-output*"), origOut)
		scope.Set(slip.Symbol("*repl-warning-prefix*"), origWarn)
	})
	scope.Set(slip.Symbol("*repl-warning-prefix*"), slip.String("WARN "))
	var out strings.Builder
	scope.Set(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	config := `(setq *repl-key-bindings* '(("C-g" . no-such-action)))
`
	tt.Nil(t, os.WriteFile(keyConfigFile, []byte(config), 0666))
	stdout := captureStdout(t, func() {
		defer func() {
			if r := recover(); r != nil {
				t.Errorf("loading the config failed: %v", r)
			}
		}()
		repl.SetConfigDir("config")
	})
	warning := out.String() + stdout
	tt.Equal(t, true, strings.Contains(warning, "WARN warning: dropped"), "warning %q", warning)
	tt.Equal(t, false, strings.Contains(warning, "\x1b"), "warning %q", warning)
}

// Addendum A item 3: *print-level*, *print-length*, *print-lines* and
// *print-escape* must not change what is written for the bindings.
func TestReplKeyBindingsPersistIgnoresPrintVars(t *testing.T) {
	keyConfigSetup(t)
	vars := []string{"*print-level*", "*print-length*", "*print-lines*", "*print-escape*"}
	orig := map[string]string{}
	for _, v := range vars {
		orig[v] = keyEval(t, v)
	}
	t.Cleanup(func() {
		for _, v := range vars {
			_ = keyEval(t, "(setq "+v+" "+orig[v]+")")
		}
	})
	_ = keyEval(t, `(progn
                      (setq *print-level* 1)
                      (setq *print-length* 2)
                      (setq *print-lines* 1)
                      (setq *print-escape* nil))`)
	_ = keyEval(t, `(progn
                      (repl-bind-key "C-l" 'line-end)
                      (repl-bind-key "M-[1;5C" 'back-word)
                      (repl-bind-key "C-\\" 'forward-word)
                      (repl-bind-key "C-a" nil))`)
	const want = `(("C-l" . line-end) ("M-[1;5C" . back-word) ("C-\\" . forward-word) ("C-a"))`
	config := readKeyConfig(t)
	tt.Equal(t, `(setq *repl-key-bindings* '`+want+`)`, keyBindingsLine(t, config))

	// Reload the saved file into cleared bindings.
	_ = keyEval(t, `(setq *repl-key-bindings* nil)`)
	tt.Nil(t, os.WriteFile(keyConfigFile, []byte(config), 0666))
	repl.SetConfigDir("config")
	for _, v := range vars {
		_ = keyEval(t, "(setq "+v+" "+orig[v]+")")
	}
	// Printed without escapes the backslash is not doubled.
	tt.Equal(t, strings.ReplaceAll(want, `\\`, `\`), keyEval(t, `*repl-key-bindings*`))
	tt.Equal(t, "forward-word", keyEval(t, `(repl-key-binding "C-\\")`))
}

// Addendum A item 8: the cost of replacing all bindings, which rebuilds the
// user layer. Each setq also rewrites the config file when a config
// directory has been set, as it has in this package, so that cost is
// included.
func BenchmarkReplKeyBindingsSetq(b *testing.B) {
	scope := slip.NewScope()
	code := slip.ReadString(`(setq *repl-key-bindings* '(
  ("C-l" . line-end) ("C-g" . line-end) ("C-q" . line-end) ("C-x" . line-end)
  ("C-z" . line-end) ("C-@" . line-end) ("C-]" . line-end) ("C-^" . line-end)
  ("M-a" . line-end) ("M-c" . line-end) ("M-g" . line-end) ("M-i" . line-end)
  ("M-j" . line-end) ("M-k" . line-end) ("M-l" . line-end) ("M-m" . line-end)
  ("M-o" . line-end) ("M-q" . line-end) ("M-x" . line-end) ("M-[1;5E" . line-end)))`, scope)
	defer slip.ReadString(`(setq *repl-key-bindings* nil)`, scope).Eval(scope, nil)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		code.Eval(scope, nil)
	}
}
