// Copyright (c) 2022, Peter Ohler, All rights reserved.

// Package pkg pulls in all sub-packages.
package pkg

import (
	// Pull in basic functions.
	_ "github.com/ohler55/slip/pkg/basic"

	// Pull in flavors functions and type.
	_ "github.com/ohler55/slip/pkg/flavors"

	// Pull in hash-table functions and type.
	_ "github.com/ohler55/slip/pkg/hash"

	// Pull in list functions.
	_ "github.com/ohler55/slip/pkg/list"
)
