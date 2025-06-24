// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"bytes"
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSnapshotBasicNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(snapshot nil)`,
		Expect: `/\(setq common-lisp::\*print-readably\* nil\)/`,
	}).Test(t)
}

func TestSnapshotBasicWriter(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-output-to-string (s) (snapshot s))`,
		Expect: `/\(setq common-lisp::\*print-readably\* nil\)/`,
	}).Test(t)
}

func TestSnapshotBasicFile(t *testing.T) {
	defer func() { _ = os.RemoveAll("testdata/snapshot.lisp") }()
	(&sliptest.Function{
		Source: `(snapshot "testdata/snapshot.lisp")`,
		Expect: `nil`,
	}).Test(t)
	content, err := os.ReadFile("testdata/snapshot.lisp")
	tt.Nil(t, err)
	tt.Equal(t, true, bytes.Contains(content, []byte("(setq common-lisp::*print-readably* nil)")))
}

func TestSnapshotRequire(t *testing.T) {
	(&sliptest.Function{
		Source: `(progn (require 'testplugin "../cl/testplugin") (snapshot nil))`,
		Expect: `/\(require 'testplugin "../cl/testplugin"\)/`,
	}).Test(t)
}

func TestSnapshotPackage(t *testing.T) {
	defer func() {
		if p := slip.FindPackage("snap-pack-1"); p != nil {
			slip.RemovePackage(p)
		}
		if p := slip.FindPackage("snap-pack-2"); p != nil {
			slip.RemovePackage(p)
		}
	}()
	(&sliptest.Function{
		Source: `(progn
                  (defpackage 'snap-pack-1
                    (:documentation "snappy package one")
                    (:nicknames "snap-1")
                    (:use "common-lisp"))
                  (defpackage 'snap-pack-2
                    (:nicknames "snap-2")
                    (:export "snap" "pack")
                    (:use "common-lisp"))
                  (snapshot nil))`,
		Validate: func(t *testing.T, v slip.Object) {
			ss, _ := v.(slip.String)
			str := string(ss)
			tt.Equal(t, `/\(defpackage "snap-pack-1"/`, str)
			tt.Equal(t, `/\(:documentation "snappy package one"\)/`, str)
			tt.Equal(t, `/\(:nicknames "snap-1"\)/`, str)
			tt.Equal(t, `/\(:use "common-lisp"\)\)/`, str)

			tt.Equal(t, `/\(defpackage "snap-pack-2"/`, str)
			tt.Equal(t, `/\(:export "snap" "pack"\)\)/`, str)
		},
	}).Test(t)
}

func TestSnapshotConstant(t *testing.T) {
	defer func() {
		slip.UserPkg.Remove("quux")
		slip.UserPkg.Remove("duux")
		if p := slip.FindPackage("snap-pack"); p != nil {
			slip.RemovePackage(p)
		}
	}()
	(&sliptest.Function{
		Source: `(let ((p (defpackage 'snap-pack (:use "common-lisp"))))
                  (defconstant quux 3 "quack quack")
                  (defconstant duux 4 "duck duck")
                  (defconstant snap-pack::snoop 5)
                  (snapshot nil))`,
		Validate: func(t *testing.T, v slip.Object) {
			ss, _ := v.(slip.String)
			str := string(ss)
			tt.Equal(t, `/\(defconstant common-lisp-user::quux 3
  "quack quack"\)/`, str)
			tt.Equal(t, `/\(defconstant common-lisp-user::duux 4
  "duck duck"\)/`, str)
			tt.Equal(t, `/\(defconstant snap-pack::snoop 5\)/`, str)
		},
	}).Test(t)
}

func TestSnapshotVar(t *testing.T) {
	defer func() {
		slip.UserPkg.Remove("quux")
		slip.UserPkg.Remove("duux")
	}()
	(&sliptest.Function{
		Source: `(progn
                  (defvar quux 3 "quack quack")
                  (defvar duux 4 "duck duck")
                  (snapshot nil))`,
		Validate: func(t *testing.T, v slip.Object) {
			ss, _ := v.(slip.String)
			str := string(ss)
			tt.Equal(t, `/\(defvar common-lisp-user::quux nil
  "quack quack"\)/`, str)
			tt.Equal(t, `/\(setq common-lisp-user::quux 3\)/`, str)
			tt.Equal(t, `/\(defvar common-lisp-user::duux nil
  "duck duck"\)/`, str)
			tt.Equal(t, `/\(setq common-lisp-user::duux 4\)/`, str)
		},
	}).Test(t)
}

func TestSnapshotFlavors(t *testing.T) {
	defer func() {
		scope := slip.NewScope()
		_ = slip.ReadString(`(undefflavor 'snapper)`, scope).Eval(scope, nil)
		_ = slip.ReadString(`(undefflavor 'snappy)`, scope).Eval(scope, nil)
	}()
	(&sliptest.Function{
		Source: `(progn
                  (defflavor snappy (x) ())
                  (defflavor snapper (y) (snappy))
                  (snapshot nil))`,
		Validate: func(t *testing.T, v slip.Object) {
			ss, _ := v.(slip.String)
			str := string(ss)
			tt.Equal(t, `/\(defflavor snappy \(x\)/`, str)
			tt.Equal(t, `/\(defflavor snapper \(y\)/`, str)
		},
	}).Test(t)
}

func TestSnapshotFunctions(t *testing.T) {
	defer func() {
		slip.CurrentPackage.Undefine("snap-1")
		slip.CurrentPackage.Undefine("snap-2")
	}()
	(&sliptest.Function{
		Source: `(defun snap-1 (x) (1+ x))
                 (defun snap-2 (x) (+ x 2))
                 (snapshot nil)`,
		Validate: func(t *testing.T, v slip.Object) {
			ss, _ := v.(slip.String)
			str := string(ss)
			tt.Equal(t, `/\(defun snap-1 \(x\)
  \(1\+ x\)\)/`, str)
			tt.Equal(t, `/\(defun snap-2 \(x\)
  \(\+ x 2\)\)/`, str)
		},
	}).Test(t)
}

// TBD functions

func TestSnapshotBadDestination(t *testing.T) {
	(&sliptest.Function{
		Source:    `(snapshot 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSnapshotWriteError(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(let ((*standard-output* out)) (snapshot t))`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}

func TestSnapshotFileCreateError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(snapshot "testdata/sister")`,
		PanicType: slip.FileErrorSymbol,
	}).Test(t)
}
