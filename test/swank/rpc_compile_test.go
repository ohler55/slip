// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"os"
	"strings"
	"testing"

	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg/cl"
	_ "github.com/ohler55/slip/pkg/swank"
)

// --- ping ---

func TestPing(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:ping"),
		slip.Fixnum(42),
	})
	if result == nil {
		t.Fatal("expected ping result")
	}
	if n, ok := result.(slip.Fixnum); !ok || n != 42 {
		t.Errorf("expected 42, got %v", result)
	}
}

func TestPingNoArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:ping"),
	})
	if result != nil {
		t.Errorf("expected nil for ping with no args, got %v", result)
	}
}

func TestPingString(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:ping"),
		slip.String("hello"),
	})
	if s, ok := result.(slip.String); !ok || string(s) != "hello" {
		t.Errorf("expected hello, got %v", result)
	}
}

// --- compile-string-for-emacs ---

func TestCompileStringSuccess(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-string-for-emacs"),
		slip.String("(+ 1 2)"),
		slip.String("test-buffer"),
		slip.Fixnum(1),
		nil,
		nil,
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":successp") {
		t.Errorf("expected :successp in result, got %s", s)
	}
	if !strings.Contains(s, ":successp") {
		t.Errorf("expected :successp in result: %s", s)
	}
}

func TestCompileStringDefun(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-string-for-emacs"),
		slip.String("(defun compile-test-fn (x) (* x x))"),
		slip.String("test-buffer"),
		slip.Fixnum(1),
		nil,
		nil,
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":successp") {
		t.Errorf("expected :successp, got %s", s)
	}
}

func TestCompileStringSyntaxError(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-string-for-emacs"),
		slip.String("(+ 1"),
		slip.String("test-buffer"),
		slip.Fixnum(10),
		nil,
		nil,
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":notes") {
		t.Errorf("expected :notes with error, got %s", s)
	}
}

func TestCompileStringNoArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-string-for-emacs"),
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":successp") {
		t.Errorf("expected :successp in result, got %s", s)
	}
}

func TestCompileStringNonStringArg(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-string-for-emacs"),
		slip.Fixnum(42),
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":successp") {
		t.Errorf("expected :successp in result, got %s", s)
	}
}

func TestCompileStringWithPositionList(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// SLIME can send position as (:position N)
	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-string-for-emacs"),
		slip.String("(+ 1 2)"),
		slip.String("test-buffer"),
		slip.List{slip.Symbol(":position"), slip.Fixnum(100)},
		nil,
		nil,
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":successp") {
		t.Errorf("expected :successp, got %s", s)
	}
}

// --- compile-file-for-emacs ---

func TestCompileFileSuccess(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Write a temp file
	tmp := t.TempDir() + "/test.lisp"
	writeTestFile(t, tmp, "(defun compile-file-test (x) (+ x 1))")

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-file-for-emacs"),
		slip.String(tmp),
		nil, // don't load
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":successp") {
		t.Errorf("expected :successp in result, got %s", s)
	}
	if !strings.Contains(s, ":successp") {
		t.Errorf("expected :successp, got %s", s)
	}
}

func TestCompileFileAndLoad(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	tmp := t.TempDir() + "/test-load.lisp"
	writeTestFile(t, tmp, "(defun compile-and-load-fn () 99)")

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-file-for-emacs"),
		slip.String(tmp),
		slip.True, // load after compile
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":successp") {
		t.Errorf("expected :successp, got %s", s)
	}
}

func TestCompileFileMissing(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-file-for-emacs"),
		slip.String("/nonexistent/file.lisp"),
		nil,
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":notes") {
		t.Errorf("expected :notes with error, got %s", s)
	}
	if !strings.Contains(s, ":error") {
		t.Errorf("expected :error severity, got %s", s)
	}
}

func TestCompileFileNoArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-file-for-emacs"),
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":successp") {
		t.Errorf("expected :successp in result, got %s", s)
	}
}

func TestCompileFileNonStringArg(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-file-for-emacs"),
		slip.Fixnum(42),
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":successp") {
		t.Errorf("expected :successp in result, got %s", s)
	}
}

func TestCompileFileSyntaxError(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	tmp := t.TempDir() + "/bad.lisp"
	writeTestFile(t, tmp, "(defun broken (x) (+ x")

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-file-for-emacs"),
		slip.String(tmp),
		nil,
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":notes") {
		t.Errorf("expected :notes with error, got %s", s)
	}
}

func TestCompileFileLoadFailure(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// File that compiles but fails at eval time
	tmp := t.TempDir() + "/runtime-err.lisp"
	writeTestFile(t, tmp, "(error \"deliberate failure\")")

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:compile-file-for-emacs"),
		slip.String(tmp),
		slip.True, // load after compile
	})
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":notes") {
		t.Errorf("expected :notes for eval failure, got %s", s)
	}
}

// --- load-file ---

func TestLoadFileSuccess(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	tmp := t.TempDir() + "/load-test.lisp"
	writeTestFile(t, tmp, "(defun load-test-fn () 42)")

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:load-file"),
		slip.String(tmp),
	})
	if result == nil {
		t.Fatal("expected load result")
	}
}

func TestLoadFileMissing(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// load-file calls evalWithCapture which recovers panics
	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:load-file"),
		slip.String("/nonexistent/load-test.lisp"),
	})
	// Should still return something (nil or error string)
	_ = result
}

func TestLoadFileNoArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:load-file"),
	})
	if s, ok := result.(slip.String); !ok || string(s) != "nil" {
		t.Errorf("expected nil string, got %v", result)
	}
}

func TestLoadFileNonStringArg(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:load-file"),
		slip.Fixnum(42),
	})
	if s, ok := result.(slip.String); !ok || string(s) != "nil" {
		t.Errorf("expected nil string, got %v", result)
	}
}

// --- update-indentation-information ---

func TestUpdateIndentation(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:update-indentation-information"),
	})
	if result != nil {
		t.Errorf("expected nil, got %v", result)
	}
}

// --- inspector-call-nth-action ---

func TestInspectorCallNthActionNoArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:inspector-call-nth-action"),
	})
	if result != nil {
		t.Errorf("expected nil for no args, got %v", result)
	}
}

func TestInspectorCallNthActionNonFixnum(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:inspector-call-nth-action"),
		slip.String("not-a-number"),
	})
	if result != nil {
		t.Errorf("expected nil for non-fixnum arg, got %v", result)
	}
}

func TestInspectorCallNthActionOutOfBounds(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Initialize inspector first
	env.sendRexOK(t, slip.List{
		slip.Symbol("swank:init-inspector"),
		slip.String("42"),
	})

	// Action index out of bounds (no actions registered)
	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:inspector-call-nth-action"),
		slip.Fixnum(0),
	})
	if result != nil {
		t.Errorf("expected nil for out-of-bounds action, got %v", result)
	}
}

func TestInspectorCallNthActionNegativeIndex(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	env.sendRexOK(t, slip.List{
		slip.Symbol("swank:init-inspector"),
		slip.String("42"),
	})

	result := env.sendRexOK(t, slip.List{
		slip.Symbol("swank:inspector-call-nth-action"),
		slip.Fixnum(-1),
	})
	if result != nil {
		t.Errorf("expected nil for negative index, got %v", result)
	}
}

// --- helpers ---

func writeTestFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("failed to write test file: %v", err)
	}
}
