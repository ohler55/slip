// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPackage(t *testing.T) {
	pkgs := slip.PackageNames()
	for _, want := range []slip.String{
		"bag",
		"common-lisp",
		"common-lisp-user",
		"flavors",
		"gi",
		"keyword",
		"test",
	} {
		var has bool
		for _, name := range pkgs {
			if name == want {
				has = true
				break
			}
		}
		tt.Equal(t, true, has)
	}
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

func checkUserPkgSimplify(t *testing.T, simple any) {
	uses, _ := jp.C("uses").First(simple).([]any)
	for _, want := range []string{
		"bag",
		"common-lisp",
		"flavors",
		"gi",
		"keyword",
		"test",
	} {
		var has bool
		for _, name := range uses {
			if name == want {
				has = true
				break
			}
		}
		tt.Equal(t, true, has)
	}
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
	tt.Panic(t, func() { slip.CLPkg.Set("*common-lisp*", slip.True) })
}

func checkCLPkgSimplify(t *testing.T, simple any) {
	tt.Equal(t, []any{}, jp.C("uses").First(simple))
	tt.Equal(t, "common-lisp-user", jp.C("vars").C("*package*").C("val").First(simple))
}

func TestPackageCLGensymCounter(t *testing.T) {
	cl := slip.FindPackage("common-lisp")
	tt.NotNil(t, cl)
	v, has := cl.Get("*gensym-counter*")
	orig, _ := v.(slip.Fixnum)
	tt.Equal(t, true, has)
	tt.SameType(t, slip.Fixnum(0), orig)
	_ = cl.Set("*gensym-counter*", orig+1)
	current, _ := cl.Get("*gensym-counter*")
	tt.Equal(t, orig+1, current)
	tt.Panic(t, func() { _ = cl.Set("*gensym-counter*", nil) })
}

func TestPackageCLTraceOutput(t *testing.T) {
	cl := slip.FindPackage("common-lisp")
	tt.NotNil(t, cl)
	out, has := cl.Get("*trace-output*")
	tt.Equal(t, true, has)
	_ = cl.Set("*trace-output*", out)
	tt.Panic(t, func() { _ = cl.Set("*trace-output*", nil) })
}

func TestPackageKeyword(t *testing.T) {
	kp := slip.FindPackage("keyword")
	tt.NotNil(t, kp)
	tt.Panic(t, func() { kp.Set("", slip.True) })
	tt.Panic(t, func() { kp.Set(":yes", slip.True) })
	// Should not panic.
	kp.Set(":yes", slip.Symbol(":yes"))
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

func TestPackageVars(t *testing.T) {
	key := "package-var-test"
	slip.CurrentPackage.Set(key, slip.Fixnum(7))
	val, has := slip.CurrentPackage.Get(key)
	tt.Equal(t, true, has)
	tt.Equal(t, "7", slip.ObjectString(val))

	slip.CurrentPackage.Set(key, nil)
	val, has = slip.CurrentPackage.Get(key)
	tt.Equal(t, true, has)
	tt.Nil(t, val)

	slip.CurrentPackage.Remove(key)
	val, has = slip.CurrentPackage.Get(key)
	tt.Equal(t, false, has)
	tt.Nil(t, val)

	tt.Panic(t, func() { slip.CLPkg.Remove("*print-pretty*") })

	cl := slip.FindPackage("common-lisp")
	tt.Panic(t, func() { cl.Set(key, slip.Fixnum(7)) })
	tt.Panic(t, func() { cl.Remove(key) })
	val, has = cl.Get(key)
	tt.Equal(t, false, has)
	tt.Nil(t, val)
}

func TestPackageDescribe(t *testing.T) {
	out := slip.CurrentPackage.Describe(nil, 0, 40, false)
	tt.Equal(t, true, bytes.Contains(out, []byte("Name: common-lisp-user")))

	cl := slip.FindPackage("common-lisp")
	out = cl.Describe(nil, 0, 40, false)
	tt.Equal(t, true, bytes.Contains(out, []byte("Name: common-lisp")))
	tt.Equal(t, true, bytes.Contains(out, []byte("*print-case* = ")))

	p := slip.Package{
		Name:    "fake",
		Vars:    map[string]*slip.VarVal{},
		Imports: map[string]*slip.Import{},
		Lambdas: map[string]*slip.Lambda{},
		Funcs:   map[string]*slip.FuncInfo{},
	}
	p.Import(cl, "car")
	out = p.Describe(nil, 0, 40, false)
	tt.Equal(t, true, bytes.Contains(out, []byte("Name: fake")))
}

// Dummy is the struct for a test function.
type Dummy struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Dummy) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return nil
}

func TestPackageDefine(t *testing.T) {
	cf := func(args slip.List) slip.Object {
		f := Dummy{Function: slip.Function{Name: "dummy", Args: args}}
		f.Self = &f
		return &f
	}
	doc := slip.FuncDoc{
		Name:   "dummy",
		Args:   []*slip.DocArg{},
		Return: "object",
	}
	var out strings.Builder
	orig := slip.ErrorOutput
	defer func() { slip.ErrorOutput = orig }()
	slip.ErrorOutput = &slip.OutputStream{Writer: &out}

	slip.CurrentPackage.Define(cf, &doc)
	slip.CurrentPackage.Define(cf, &doc)
	tt.Equal(t, "Warning: redefining dummy\n", out.String())
}
