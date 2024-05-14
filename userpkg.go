// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

var (
	// UserPkg is the common-lisp-user package.
	UserPkg = Package{
		Name:      "common-lisp-user",
		Nicknames: []string{"cl-user", "user"},
		Doc:       "The default package for user code and variables.",
		Imports:   map[string]*Import{},
		PreSet:    DefaultPreSet,
	}
)

func init() {
	UserPkg.Initialize(nil)
	UserPkg.Use(&CLPkg)
	_ = UserPkg.Set("*common-lisp-user*", &UserPkg)
	UserPkg.Export("*common-lisp-user*")
}
