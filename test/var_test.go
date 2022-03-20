// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"os"
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

func TestVar(t *testing.T) {
	key := slip.Symbol("testing")
	_, has := slip.GetVar(key)
	require.False(t, has)

	slip.SetVar(key, slip.Fixnum(7))
	var v slip.Object
	v, has = slip.GetVar(key)
	require.True(t, has)
	require.Equal(t, slip.Fixnum(7), v)

	has = slip.HasVar(key)
	require.True(t, has)

	slip.RemoveVar(key)
	has = slip.HasVar(key)
	require.False(t, has)

	has = slip.HasVar(slip.Symbol("*print-base*"))
	require.True(t, has)

}

func TestSetConstant(t *testing.T) {
	key := slip.Symbol("*just-a-test*")
	_, has := slip.GetVar(key)
	require.False(t, has)

	slip.DefConstant(key, slip.True, "is just for testing.")
	require.Panics(t, func() { slip.DefConstant(key, nil, "is just for testing.") })

	var v slip.Object
	v, has = slip.GetVar(key)
	require.True(t, has)
	require.Equal(t, slip.True, v)

	has = slip.HasVar(key)
	require.True(t, has)

	require.Panics(t, func() { slip.SetVar(key, nil) })

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	doc = slip.DescribeVar(slip.Symbol("not-found"))
	require.Equal(t, "", doc)

}

func TestDefaultPathnameDefaults(t *testing.T) {
	key := slip.Symbol("*default-pathname-defaults*")
	v, has := slip.GetVar(key)
	require.True(t, has)
	path, ok := v.(slip.String)
	require.True(t, ok)
	require.True(t, strings.HasSuffix(string(path), "/test"))
	defer slip.SetVar(key, path)

	slip.SetVar(key, slip.String("/tmp"))
	v, has = slip.GetVar(key)
	require.True(t, has)
	path2, ok2 := v.(slip.String)
	require.True(t, ok2)
	require.Equal(t, "/tmp", string(path2))

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	require.Panics(t, func() { slip.SetVar(key, slip.True) })
}

func TestErrorOutput(t *testing.T) {
	key := slip.Symbol("*error-output*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	require.Regexp(t, "^#<FILE-STREAM .*stderr.*>", slip.ObjectString(orig))

	defer slip.SetVar(key, orig)

	slip.SetVar(key, (*slip.FileStream)(os.Stdout))
	var v slip.Object
	v, has = slip.GetVar(key)
	require.True(t, has)
	require.Regexp(t, "^#<FILE-STREAM .*stdout.*>", slip.ObjectString(v))

	require.Panics(t, func() { slip.SetVar(key, slip.True) })
}

func TestStandardOutput(t *testing.T) {
	key := slip.Symbol("*standard-output*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	require.Regexp(t, "^#<FILE-STREAM .*stdout.*>", slip.ObjectString(orig))

	defer slip.SetVar(key, orig)

	slip.SetVar(key, (*slip.FileStream)(os.Stderr))
	var v slip.Object
	v, has = slip.GetVar(key)
	require.True(t, has)
	require.Regexp(t, "^#<FILE-STREAM .*stderr.*>", slip.ObjectString(v))

	require.Panics(t, func() { slip.SetVar(key, slip.True) })
}

func TestStandardInput(t *testing.T) {
	key := slip.Symbol("*standard-input*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	require.Regexp(t, "^#<FILE-STREAM .*stdin.*>", slip.ObjectString(orig))

	defer slip.SetVar(key, orig)

	slip.SetVar(key, (*slip.FileStream)(os.Stderr))
	var v slip.Object
	v, has = slip.GetVar(key)
	require.True(t, has)
	require.Regexp(t, "^#<FILE-STREAM .*stderr.*>", slip.ObjectString(v))

	require.Panics(t, func() { slip.SetVar(key, slip.True) })
}

func TestDescribeFunction(t *testing.T) {
	doc := slip.DescribeFunction(slip.Symbol("car"))
	require.NotNil(t, doc)
	require.Equal(t, "car", doc.Name)

	doc = slip.DescribeFunction(slip.Symbol("not-a-function"))
	require.Nil(t, doc)
}
