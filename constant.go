// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
)

var (
	constantValues = map[string]Object{}
	constantDocs   = map[string]string{}
)

// DefConstant defines a constant with a value and documentation. It is
// usually called by an init() function at startup.
func DefConstant(sym Symbol, value Object, doc string) {
	name := strings.ToLower(string(sym))
	if _, has := constantValues[name]; has {
		panic(fmt.Sprintf("%s is already defined", sym))
	}
	constantValues[name] = value
	constantDocs[name] = doc
}
