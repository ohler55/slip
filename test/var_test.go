// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"os"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestVar(t *testing.T) {
	key := slip.Symbol("testing")
	_, has := slip.GetVar(key)
	tt.Equal(t, false, has)

	slip.SetVar(key, slip.Fixnum(7))
	var v slip.Object
	v, has = slip.GetVar(key)
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Fixnum(7), v)

	has = slip.HasVar(key)
	tt.Equal(t, true, has)

	slip.RemoveVar(key)
	has = slip.HasVar(key)
	tt.Equal(t, false, has)

	has = slip.HasVar(slip.Symbol("*print-base*"))
	tt.Equal(t, true, has)
}

func TestSetConstant(t *testing.T) {
	key := slip.Symbol("*just-a-test*")
	_, has := slip.GetVar(key)
	tt.Equal(t, false, has)

	slip.DefConstant(slip.CurrentPackage, string(key), slip.True, "is just for testing.")
	tt.Panic(t, func() { slip.DefConstant(slip.CurrentPackage, string(key), nil, "is just for testing.") })

	var v slip.Object
	v, has = slip.GetVar(key)
	tt.Equal(t, true, has)
	tt.Equal(t, slip.True, v)

	has = slip.HasVar(key)
	tt.Equal(t, true, has)

	tt.Panic(t, func() { slip.SetVar(key, nil) })

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	doc = slip.DescribeVar(slip.Symbol("not-found"))
	tt.Equal(t, "", doc)
}

func TestDefaultPathnameDefaults(t *testing.T) {
	key := slip.Symbol("*default-pathname-defaults*")
	v, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	path, ok := v.(slip.String)
	tt.Equal(t, true, ok)
	tt.Equal(t, true, strings.HasSuffix(string(path), "/test"))
	defer slip.SetVar(key, path)

	slip.SetVar(key, slip.String("/tmp"))
	v, has = slip.GetVar(key)
	tt.Equal(t, true, has)
	path2, ok2 := v.(slip.String)
	tt.Equal(t, true, ok2)
	tt.Equal(t, "/tmp", string(path2))

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	tt.Panic(t, func() { slip.SetVar(key, slip.True) })
}

func TestErrorOutput(t *testing.T) {
	key := slip.Symbol("*error-output*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	tt.Equal(t, "/^#<FILE-STREAM .*stderr.*>/", slip.ObjectString(orig))

	defer slip.SetVar(key, orig)

	slip.SetVar(key, (*slip.FileStream)(os.Stdout))
	var v slip.Object
	v, has = slip.GetVar(key)
	tt.Equal(t, true, has)
	tt.Equal(t, "/^#<FILE-STREAM .*stdout.*>/", slip.ObjectString(v))

	tt.Panic(t, func() { slip.SetVar(key, slip.True) })
}

func TestStandardOutput(t *testing.T) {
	key := slip.Symbol("*standard-output*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	tt.Equal(t, "/^#<FILE-STREAM .*stdout.*>/", slip.ObjectString(orig))

	defer slip.SetVar(key, orig)

	slip.SetVar(key, (*slip.FileStream)(os.Stderr))
	var v slip.Object
	v, has = slip.GetVar(key)
	tt.Equal(t, true, has)
	tt.Equal(t, "/^#<FILE-STREAM .*stderr.*>/", slip.ObjectString(v))

	tt.Panic(t, func() { slip.SetVar(key, slip.True) })
}

func TestStandardInput(t *testing.T) {
	key := slip.Symbol("*standard-input*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	tt.Equal(t, "/^#<FILE-STREAM .*stdin.*>/", slip.ObjectString(orig))

	defer slip.SetVar(key, orig)

	slip.SetVar(key, (*slip.FileStream)(os.Stderr))
	var v slip.Object
	v, has = slip.GetVar(key)
	tt.Equal(t, true, has)
	tt.Equal(t, "/^#<FILE-STREAM .*stderr.*>/", slip.ObjectString(v))

	tt.Panic(t, func() { slip.SetVar(key, slip.True) })
}

func TestDescribeFunction(t *testing.T) {
	doc := slip.DescribeFunction(slip.Symbol("car"))
	tt.NotNil(t, doc)
	tt.Equal(t, "car", doc.Name)

	doc = slip.DescribeFunction(slip.Symbol("not-a-function"))
	tt.Nil(t, doc)
}
