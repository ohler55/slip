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
	require.Equal(t, `("COMMON-LISP" "COMMON-LISP-USER")`, pkgs.String())
}

func TestPackageUser(t *testing.T) {
	(&sliptest.Object{
		Target:    &slip.UserPkg,
		String:    `#<package "COMMON-LISP-USER">`,
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
	require.Equal(t, []interface{}{"COMMON-LISP"}, jp.C("uses").First(simple))
	require.Equal(t, "COMMON-LISP-USER", jp.C("vars").C("*PACKAGE*").C("val").First(simple))
}

func TestPackageCL(t *testing.T) {
	(&sliptest.Object{
		Target:    &slip.CLPkg,
		String:    `#<package "COMMON-LISP">`,
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
	require.Equal(t, "COMMON-LISP-USER", jp.C("vars").C("*PACKAGE*").C("val").First(simple))
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
  imports: {BB: {name: BB pkg: "#<package \"B\">"}}
  name: C
  nicknames: [sea see]
  uses: [A]
  vars: {AAA: {doc: "" pkg: A val: 7} BB: {doc: "" pkg: B val: 3}}
}`, pretty.SEN(pc))
}

func TestPackageCurrent(t *testing.T) {
	require.Equal(t, "COMMON-LISP-USER", slip.CurrentPackage.Name)
	defer func() { slip.CurrentPackage = &slip.UserPkg }()

	pa := slip.DefPackage("a", []string{"aye"}, "Lots of ayes.")
	slip.CLPkg.Set("*package*", pa)
	require.Equal(t, "A", slip.CurrentPackage.Name)
}
