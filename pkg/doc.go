// Copyright (c) 2022, Peter Ohler, All rights reserved.

// Package pkg pulls in all sub-packages.
package pkg

import (
	// Pull in cl functions.
	_ "github.com/ohler55/slip/pkg/cl"

	// Pull in flavors functions and type.
	_ "github.com/ohler55/slip/pkg/flavors"

	// Pull in gi functions and type.
	_ "github.com/ohler55/slip/pkg/gi"

	// Pull in bag functions.
	_ "github.com/ohler55/slip/pkg/bag"

	// Pull in test functions.
	_ "github.com/ohler55/slip/pkg/test"
)
