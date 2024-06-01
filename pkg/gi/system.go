// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var system *flavors.Flavor

func defSystem() {
	Pkg.Initialize(nil)
	system = flavors.DefFlavor(
		"system",
		map[string]slip.Object{
			"author":         nil,
			"maintainer":     nil,
			"license":        nil,
			"version":        nil,
			"homepage":       nil,
			"bug-tracker":    nil,
			"source-control": nil,
			"description":    nil,
			"depends-on":     nil,
			"components":     nil,
			"in-order-to":    nil,
			"scratch":        nil,
			"cache":          nil,
		},
		nil, // inherit
		slip.List{ // options
			slip.Symbol(":inittable-instance-variables"),
			slip.Symbol(":gettable-instance-variables"),
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`Instances of this Flavor define a system similar to ASDF in common LISP
(https://asdf.common-lisp.dev) but with some differences. A __system__ instance
captures the information associated with a code that implements the
system. This includes providence variables such as author, version, and source
location to name a few.

Some of the differences when compared to ASDF are:

 - Not all depends-on sources need be in a common directory.
 - An import cache is used to keep local copies of source systems.
 - Support for using git repositories is included.
 - The :components variable only allows for files and not modules.
 - The :depends-on variable differs to support git and other system source.
 - The :in-order-to variable differs although it serves the same purpose.
 - The __system__ is a Flavor that also supports a :fetch method.
 - A :scratch and :cache variable are included.


The usual use of a __system__ instance is to first send the instance a :fetch
method to cache sources and then invoke one of the operations defined in the
:in-order-to variable.

`),
			},
			slip.List{
				slip.Symbol(":default-handler"),
				// TBD a function to parse and run the target in :in-order-to
				// define a function that given a :in-order-to spec invokes the specified code
				//  additional argument are assigned somehow, maybe with keys?
				slip.Symbol("car"),
			},
		},
		&Pkg,
	)
	system.DefMethod(":fetch", "", systemFetchCaller{})
}

type systemFetchCaller struct{}

func (caller systemFetchCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// TBD
	return nil
}

func (caller systemFetchCaller) Docs() string {
	return `__:fetch__

Fetches all sources specified in the the :depends-on variable and places them
in the cache.
`
}
