// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

var (
	// UserPkg is the COMMON-LISP-USER package.
	UserPkg = Package{
		Name:      "COMMON-LISP-USER",
		Nicknames: []string{"CL-USER", "USER"},
		Doc:       "The default package for user code and variables.",
		Vars:      map[string]*VarVal{},
		Imports:   map[string]*Import{},
		Funcs:     map[string]*FuncInfo{},
	}
)

func init() {
	UserPkg.Use(&CLPkg)
}
