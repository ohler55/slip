// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

var (
	// UserPkg is the common-lisp-user package.
	UserPkg = Package{
		Name:      "common-lisp-user",
		Nicknames: []string{"cl-user", "user"},
		Doc:       "The default package for user code and variables.",
		Vars:      map[string]*VarVal{},
		Imports:   map[string]*Import{},
		lambdas:   map[string]*Lambda{},
		funcs:     map[string]*FuncInfo{},
		PreSet:    DefaultPreSet,
	}
)

func init() {
	UserPkg.Use(&CLPkg)
	UserPkg.Set("*common-lisp-user*", &UserPkg)
}
