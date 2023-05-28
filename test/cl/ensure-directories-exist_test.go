// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/sliptest"
)

func TestEnsureDirectoriesExistOk(t *testing.T) {
	defer func() {
		_ = os.RemoveAll("testdata/one")
	}()
	(&sliptest.Function{
		Source: `(ensure-directories-exist "testdata/one/two")`,
		Expect: `/".*/testdata/one/two", t/`,
	}).Test(t)
	files, _ := filepath.Glob("testdata/one/*")
	tt.Equal(t, 1, len(files))
	tt.Equal(t, "testdata/one/two", files[0])
}

func TestEnsureDirectoriesExistPermission(t *testing.T) {
	defer func() {
		_ = os.RemoveAll("testdata/one")
	}()
	(&sliptest.Function{
		Source: `(ensure-directories-exist "testdata/one/two" :permission #o750 :verbose t)`,
		Expect: `/".*/testdata/one/two", t/`,
	}).Test(t)
	files, _ := filepath.Glob("testdata/one/*")
	tt.Equal(t, 1, len(files))
	tt.Equal(t, "testdata/one/two", files[0])
	fi, err := os.Stat("testdata/one/two")
	tt.Nil(t, err)
	tt.Equal(t, "drwxr-x---", fi.Mode().String())
	fi, err = os.Stat("testdata/one")
	tt.Nil(t, err)
	tt.Equal(t, "drwxr-x---", fi.Mode().String())
}

func TestEnsureDirectoriesExistFailMkdir(t *testing.T) {
	_ = os.WriteFile("testdata/one", []byte("0123456789"), 0666)
	defer func() {
		_ = os.RemoveAll("testdata/one")
	}()
	(&sliptest.Function{
		Source: `(ensure-directories-exist "testdata/one")`,
		Panics: true,
	}).Test(t)
}

func TestEnsureDirectoriesExistNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(ensure-directories-exist t)`,
		Panics: true,
	}).Test(t)
}

func TestEnsureDirectoriesExistPermNotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(ensure-directories-exist "testdata/one" :permission t)`,
		Panics: true,
	}).Test(t)
}

func TestEnsureDirectoriesExistBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(ensure-directories-exist "testdata/one" :bad t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(ensure-directories-exist "testdata/one" t t)`,
		Panics: true,
	}).Test(t)
}

func TestEnsureDirectoriesExistNoKeyArg(t *testing.T) {
	(&sliptest.Function{
		Source: `(ensure-directories-exist "testdata/one" :verbose)`,
		Panics: true,
	}).Test(t)
}
