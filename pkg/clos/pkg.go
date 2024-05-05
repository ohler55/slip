// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

var (
	// Pkg is the Class package.
	Pkg = slip.Package{
		Name:      "clos",
		Nicknames: []string{"clos"},
		Doc:       "Home of symbols defined for the CLOS functions, variables, and constants.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*clos*": {Val: &Pkg, Doc: Pkg.Doc},
		},
		&ClassName{},
	)
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}

// MethodDocFromFunc creates docs for a method from a documented function.
func MethodDocFromFunc(method, funcName, flavor, obj string) string {
	var b []byte
	fd := slip.DescribeFunction(slip.Symbol(funcName))
	if fd != nil {
		b = fmt.Appendf(b, "__%s__ ", method)
		for _, da := range fd.Args[1:] { // first arg is always the instance
			if da.Name[0] == '&' {
				b = fmt.Appendf(b, "%s ", da.Name)
			} else {
				b = fmt.Appendf(b, "_%s_ ", da.Name)
			}
		}
		if 0 < len(fd.Return) {
			b = fmt.Appendf(b, "=> _%s_\n", fd.Return)
		} else {
			b = append(b, '\n')
		}
		for _, da := range fd.Args[1:] {
			if da.Name[0] != '&' {
				b = fmt.Appendf(b, "   _%s_ [%s] %s\n", da.Name, da.Type, da.Text)
			}
		}
		b = fmt.Appendf(b, "\n\nThe __%s__ method", method)
		b = append(b, fd.Text[strings.IndexByte(fd.Text, ' '):]...)
		if 0 < len(fd.Examples) {
			b = append(b, "\n\n\nExamples:\n"...)
			pat := fmt.Sprintf("(%s %s", funcName, obj)
			rep := fmt.Sprintf("(send %s %s", obj, method)
			for _, ex := range fd.Examples {
				b = append(b, ' ', ' ')
				b = append(b, strings.Replace(ex, pat, rep, 1)...)
				b = append(b, '\n')
			}
		}
		b = fmt.Appendf(b, "\n\nSee also: __%s__\n", funcName)
	}
	return string(b)
}
