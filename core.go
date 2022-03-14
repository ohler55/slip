// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "math"

var (
	constValues = map[string]Object{
		"most-positive-fixnum": Fixnum(math.MaxInt64),
		"most-negative-fixnum": Fixnum(math.MinInt64),
	}

	constDocs = map[Symbol]string{
		"most-positive-fixnum": "The most positive value a _fixnum_ can have.",
		"most-negative-fixnum": "The most negative value a _fixnum_ can have.",
	}
)
