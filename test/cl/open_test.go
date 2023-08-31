// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"errors"
	"fmt"
	"io"
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestOpenBasic(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	defer func() {
		if f, _ := scope.Get(slip.Symbol("file")).(io.Closer); f != nil {
			_ = f.Close()
		}
	}()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setq file (open "testdata/load-me.lisp"))`,
		Expect: "/#<FILE-STREAM testdata/load-me.lisp {.+}>/",
	}).Test(t)
}

func TestOpenProbe(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setq file (open "testdata/load-me.lisp" :direction :probe))`,
		Expect: "/#<FILE-STREAM testdata/load-me.lisp {.+}>/",
	}).Test(t)
	f, _ := scope.Get(slip.Symbol("file")).(io.Closer)
	tt.NotNil(t, f)
	err := f.Close()
	tt.Equal(t, true, errors.Is(err, os.ErrClosed))
}

func TestOpenInput(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	defer func() {
		if f, _ := scope.Get(slip.Symbol("file")).(io.Closer); f != nil {
			_ = f.Close()
		}
	}()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setq file (open "testdata/map.sen" :direction :input))`,
		Expect: "/#<FILE-STREAM testdata/map.sen {.+}>/",
	}).Test(t)
	result := slip.ReadString(`(send (make-instance bag-flavor :read file) :write)`).Eval(scope, nil)
	tt.Equal(t, `"{a: 1 b: 2}"`, slip.ObjectString(result))
}

func TestOpenOutputCreate(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	filename := "testdata/test.out"
	defer func() {
		_ = os.Remove(filename)
	}()
	(&sliptest.Function{
		Scope:  scope,
		Source: fmt.Sprintf(`(setq file (open %q :direction :output :if-does-not-exist :create))`, filename),
		Expect: fmt.Sprintf("/#<FILE-STREAM %s {.+}>/", filename),
	}).Test(t)
	_ = slip.ReadString(`(format file "something~%")`).Eval(scope, nil)
	_ = slip.ReadString(`(close file)`).Eval(scope, nil)
	content, err := os.ReadFile(filename)
	tt.Nil(t, err)
	tt.Equal(t, "something\n", string(content))
}

func TestOpenOutputOverwrite(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	filename := "testdata/test.out"
	_ = os.WriteFile(filename, []byte("0123456789"), 0666)
	defer func() {
		_ = os.Remove(filename)
	}()
	(&sliptest.Function{
		Scope:  scope,
		Source: fmt.Sprintf(`(setq file (open %q :direction :output :if-exists :overwrite))`, filename),
		Expect: fmt.Sprintf("/#<FILE-STREAM %s {.+}>/", filename),
	}).Test(t)
	_ = slip.ReadString(`(princ "over" file)`).Eval(scope, nil)
	_ = slip.ReadString(`(close file)`).Eval(scope, nil)
	content, err := os.ReadFile(filename)
	tt.Nil(t, err)
	tt.Equal(t, "over456789", string(content))
}

func TestOpenOutputAppend(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	filename := "testdata/test.out"
	_ = os.WriteFile(filename, []byte("some"), 0666)
	defer func() {
		_ = os.Remove(filename)
	}()
	(&sliptest.Function{
		Scope:  scope,
		Source: fmt.Sprintf(`(setq file (open %q :direction :output :if-exists :append))`, filename),
		Expect: fmt.Sprintf("/#<FILE-STREAM %s {.+}>/", filename),
	}).Test(t)
	_ = slip.ReadString(`(princ "thing" file)`).Eval(scope, nil)
	_ = slip.ReadString(`(close file)`).Eval(scope, nil)
	content, err := os.ReadFile(filename)
	tt.Nil(t, err)
	tt.Equal(t, "something", string(content))
}

func TestOpenOutputExists(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	filename := "testdata/test.out"
	_ = os.WriteFile(filename, []byte("some"), 0666)
	defer func() {
		_ = os.Remove(filename)
	}()
	(&sliptest.Function{
		Scope:  scope,
		Source: fmt.Sprintf(`(setq file (open %q :direction :output :if-exists nil :if-does-not-exist :create))`, filename),
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    fmt.Sprintf(`(setq file (open %q :direction :output :if-exists :error :if-does-not-exist :create))`, filename),
		PanicType: slip.Symbol("file-error"),
	}).Test(t)
}

func TestOpenOutputSupersede(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	filename := "testdata/test.out"
	_ = os.WriteFile(filename, []byte("something"), 0666)
	defer func() {
		_ = os.Remove(filename)
	}()
	(&sliptest.Function{
		Scope:  scope,
		Source: fmt.Sprintf(`(setq file (open %q :direction :output :if-exists :supersede))`, filename),
		Expect: fmt.Sprintf("/#<FILE-STREAM %s {.+}>/", filename),
	}).Test(t)
	_ = slip.ReadString(`(princ "nothing" file)`).Eval(scope, nil)
	_ = slip.ReadString(`(close file)`).Eval(scope, nil)
	content, err := os.ReadFile(filename)
	tt.Nil(t, err)
	tt.Equal(t, "nothing", string(content))
}

func TestOpenOutputIOPermissions(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	filename := "testdata/test.out"
	defer func() {
		_ = os.Remove(filename)
	}()
	(&sliptest.Function{
		Scope:  scope,
		Source: fmt.Sprintf(`(setq file (open %q :direction :io :if-does-not-exist :create :permission #o644))`, filename),
		Expect: fmt.Sprintf("/#<FILE-STREAM %s {.+}>/", filename),
	}).Test(t)
	_ = slip.ReadString(`(princ "something" file)`).Eval(scope, nil)
	_ = slip.ReadString(`(close file)`).Eval(scope, nil)
	content, err := os.ReadFile(filename)
	tt.Nil(t, err)
	tt.Equal(t, "something", string(content))
	var fi os.FileInfo
	fi, err = os.Stat(filename)
	tt.Nil(t, err)
	tt.Equal(t, "-rw-r--r--", fi.Mode().String())
}

func TestOpenOutputRename(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	filename := "testdata/test.out"
	_ = os.WriteFile(filename, []byte("something"), 0666)
	defer func() {
		_ = os.Remove(filename)
		_ = os.Remove("testdata/test.out.bak")
	}()
	(&sliptest.Function{
		Scope:  scope,
		Source: fmt.Sprintf(`(setq file (open %q :direction :output :if-exists :rename))`, filename),
		Expect: fmt.Sprintf("/#<FILE-STREAM %s {.+}>/", filename),
	}).Test(t)
	_ = slip.ReadString(`(princ "nothing" file)`).Eval(scope, nil)
	_ = slip.ReadString(`(close file)`).Eval(scope, nil)
	content, err := os.ReadFile(filename)
	tt.Nil(t, err)
	tt.Equal(t, "nothing", string(content))
	_, err = os.Stat(filename + ".bak")
	tt.Nil(t, err)
}

func TestOpenOutputCantRename(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("file"), nil)
	filename := "testdata/test.out"
	_ = os.WriteFile(filename, []byte("something"), 0666)
	_ = os.Mkdir(filename+".bak", 0666)
	defer func() {
		_ = os.Remove(filename)
		_ = os.RemoveAll("testdata/test.out.bak")
	}()
	(&sliptest.Function{
		Scope:     scope,
		Source:    fmt.Sprintf(`(setq file (open %q :direction :output :if-exists :rename))`, filename),
		PanicType: slip.Symbol("file-error"),
	}).Test(t)
}

func TestOpenNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(open t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestOpenBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    `(open "testdata/map.sen" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(open "testdata/map.sen" :bad t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestOpenNoKeyValue(t *testing.T) {
	(&sliptest.Function{
		Source:    `(open "testdata/map.sen" :direction)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestOpenKeyBadValue(t *testing.T) {
	(&sliptest.Function{
		Source:    `(open "testdata/map.sen" :direction 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestOpenBadDirection(t *testing.T) {
	(&sliptest.Function{
		Source:    `(open "testdata/map.sen" :direction :bad)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestOpenBadIfExists(t *testing.T) {
	(&sliptest.Function{
		Source:    `(open "testdata/map.sen" :if-exists :bad)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestOpenBadIfNotExist(t *testing.T) {
	(&sliptest.Function{
		Source:    `(open "testdata/map.sen" :if-does-not-exist :bad)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestOpenBadPermissions(t *testing.T) {
	(&sliptest.Function{
		Source:    `(open "testdata/map.sen" :permission t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestOpenNotExist(t *testing.T) {
	(&sliptest.Function{
		Source:    `(open "testdata/nothing" :if-does-not-exist :error)`,
		PanicType: slip.Symbol("file-error"),
	}).Test(t)
	(&sliptest.Function{
		Source: `(open "testdata/nothing" :if-does-not-exist nil)`,
		Expect: "nil",
	}).Test(t)
}
