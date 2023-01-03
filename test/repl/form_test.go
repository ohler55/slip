// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/pkg/repl"
)

func TestFormDup(t *testing.T) {
	f := repl.Form{
		[]rune("(defun foo (x)"),
		[]rune(" (1+ x))"),
	}
	dup := f.Dup()
	tt.Equal(t, len(f), len(dup))
	for i := len(f) - 1; 0 <= i; i-- {
		tt.Equal(t, string(f[i]), string(dup[i]))
	}
}

func TestFormContains(t *testing.T) {
	f := repl.Form{
		[]rune("(defun foo (x)"),
		[]rune(" (1+ x))"),
	}
	tt.Equal(t, true, f.Contains("foo"))
	tt.Equal(t, true, f.Contains(" (1+ "))
	tt.Equal(t, false, f.Contains("boo"))
}

func TestFormString(t *testing.T) {
	f := repl.Form{
		[]rune("(defun foo (x)"),
		[]rune(" (1+ x))"),
	}
	tt.Equal(t, "(defun foo (x)\n (1+ x))\n", f.String())
}

func TestFormAppend(t *testing.T) {
	f := repl.Form{
		[]rune("(defun foo (x)"),
		[]rune(" (1+ x))"),
	}
	tt.Equal(t, "(defun foo (x)\t (1+ x))\n", string(f.Append(nil)))
}
