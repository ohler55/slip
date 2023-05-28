// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/sliptest"
)

func TestRenameFileOk(t *testing.T) {
	defer func() {
		_ = os.RemoveAll("testdata/old.txt")
		_ = os.RemoveAll("testdata/new.txt")
	}()
	_ = os.WriteFile("testdata/old.txt", []byte("0123456789"), 0666)
	(&sliptest.Function{
		Source: `(rename-file "testdata/old.txt" "testdata/new.txt")`,
		Expect: `/"testdata.new.txt", ".*old.txt", ".*new.txt"/`,
	}).Test(t)
	_, err := os.Stat("testdata/new.txt")
	tt.Nil(t, err)
	_, err = os.Stat("testdata/old.txt")
	tt.NotNil(t, err)
}

func TestRenameFileNotExist(t *testing.T) {
	(&sliptest.Function{
		Source: `(rename-file "testdata/not-me.lisp" "nothing")`,
		Panics: true,
	}).Test(t)
}

func TestRenameFileNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(rename-file t "x")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(rename-file "x" t)`,
		Panics: true,
	}).Test(t)
}
