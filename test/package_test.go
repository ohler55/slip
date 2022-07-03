// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPackage(t *testing.T) {
	pkgs := slip.PackageNames()
	tt.Equal(t, `("common-lisp" "common-lisp-user" "flavors" "gi")`, pkgs.String())
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
	tt.Equal(t, []interface{}{"common-lisp", "flavors", "gi"}, jp.C("uses").First(simple))
	tt.Equal(t, "common-lisp-user", jp.C("vars").C("*package*").C("val").First(simple))
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
	tt.Equal(t, []interface{}{}, jp.C("uses").First(simple))
	tt.Equal(t, "common-lisp-user", jp.C("vars").C("*package*").C("val").First(simple))
}

func TestPackageFind(t *testing.T) {
	tt.Equal(t, &slip.UserPkg, slip.FindPackage("COMMON-LISP-USER"))
	tt.Equal(t, &slip.UserPkg, slip.FindPackage("common-lisp-user"))
	tt.Equal(t, &slip.UserPkg, slip.FindPackage("user"))
	tt.Equal(t, (*slip.Package)(nil), slip.FindPackage("nothing"))
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

	tt.Panic(t, func() { pc.Import(pb, "x") })

	tt.Equal(t, `{
  doc: Sailing.
  functions: []
  imports: {bb: {name: bb pkg: "#<package \"b\">"}}
  name: c
  nicknames: [sea see]
  uses: [a]
  vars: {aaa: {doc: "" pkg: a val: 7} bb: {doc: "" pkg: b val: 3}}
}`, pretty.SEN(pc))
}

func TestPackageCurrent(t *testing.T) {
	tt.Equal(t, "common-lisp-user", slip.CurrentPackage.Name)
	defer func() { slip.CurrentPackage = &slip.UserPkg }()

	pa := slip.DefPackage("a", []string{"aye"}, "Lots of ayes.")
	slip.CLPkg.Set("*package*", pa)
	tt.Equal(t, "a", slip.CurrentPackage.Name)

	tt.Panic(t, func() { slip.CLPkg.Set("*package*", slip.True) })
}
