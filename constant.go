// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

var (
	constantValues = map[string]Object{}
	constantDocs   = map[string]string{}
)

// DefConstant defines a constant with a value and documentation. It is
// usually called by an init() function at startup.
func DefConstant(sym Symbol, value Object, doc string) {
	if _, has := constantValues[string(sym)]; has {
		panic(fmt.Sprintf("%s is already defined", sym))
	}
	constantValues[string(sym)] = value
	constantDocs[string(sym)] = doc
}
