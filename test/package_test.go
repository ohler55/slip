// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"

	"github.com/stretchr/testify/require"
)

func TestPackage(t *testing.T) {
	pkgs := slip.PackageNames()
	require.Equal(t, `("common-lisp" "common-lisp-user" "flavors")`, pkgs.String())
}

func TestPackageUser(t *testing.T) {
	(&sliptest.Object{
		Target:    &slip.UserPkg,
		String:    `#<package "common-lisp-user">`,
		Simple:    checkUserPkgSimplify,
		Hierarchy: "package.t",
		Equals: []*sliptest.EqTest{
			{Other: &slip.UserPkg, Expect: true},
			{Other: &slip.CLPkg, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: &slip.UserPkg,
	}).Test(t)
}

func checkUserPkgSimplify(t *testing.T, simple interface{}) {
	require.Equal(t, []interface{}{"common-lisp", "flavors"}, jp.C("uses").First(simple))
	require.Equal(t, "common-lisp-user", jp.C("vars").C("*package*").C("val").First(simple))
}

func TestPackageCL(t *testing.T) {
	(&sliptest.Object{
		Target:    &slip.CLPkg,
		String:    `#<package "common-lisp">`,
		Simple:    checkCLPkgSimplify,
		Hierarchy: "package.t",
		Equals: []*sliptest.EqTest{
			{Other: &slip.CLPkg, Expect: true},
			{Other: &slip.UserPkg, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: &slip.CLPkg,
	}).Test(t)
}

func checkCLPkgSimplify(t *testing.T, simple interface{}) {
	require.Equal(t, []interface{}{}, jp.C("uses").First(simple))
	require.Equal(t, "common-lisp-user", jp.C("vars").C("*package*").C("val").First(simple))
}

func TestPackageFind(t *testing.T) {
	require.Equal(t, &slip.UserPkg, slip.FindPackage("COMMON-LISP-USER"))
	require.Equal(t, &slip.UserPkg, slip.FindPackage("common-lisp-user"))
	require.Equal(t, &slip.UserPkg, slip.FindPackage("user"))
	require.Equal(t, (*slip.Package)(nil), slip.FindPackage("nothing"))
}

func TestPackageDef(t *testing.T) {
	pa := slip.DefPackage("a", []string{"aye"}, "Lots of ayes.")
	pb := slip.DefPackage("b", []string{"bee"}, "Buzzing around.")
	pc := slip.DefPackage("c", []string{"sea", "see"}, "Sailing.")

	pa.Set("aaa", slip.Fixnum(7))
	pb.Set("bb", slip.Fixnum(3))
	pc.Use(pa)
	pc.Use(pa)
	pc.Import(pb, "bb")

	require.Panics(t, func() { pc.Import(pb, "x") })

	require.Equal(t, `{
  doc: Sailing.
  imports: {bb: {name: bb pkg: "#<package \"b\">"}}
  name: c
  nicknames: [sea see]
  uses: [a]
  vars: {aaa: {doc: "" pkg: a val: 7} bb: {doc: "" pkg: b val: 3}}
}`, pretty.SEN(pc))
}

func TestPackageCurrent(t *testing.T) {
	require.Equal(t, "common-lisp-user", slip.CurrentPackage.Name)
	defer func() { slip.CurrentPackage = &slip.UserPkg }()

	pa := slip.DefPackage("a", []string{"aye"}, "Lots of ayes.")
	slip.CLPkg.Set("*package*", pa)
	require.Equal(t, "a", slip.CurrentPackage.Name)
}
