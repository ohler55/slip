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

	// Pull in clos functions.
	_ "github.com/ohler55/slip/pkg/clos"

	// Pull in generic functions.
	_ "github.com/ohler55/slip/pkg/generic"

	// Pull in net functions.
	_ "github.com/ohler55/slip/pkg/net"

	// Pull in test functions.
	_ "github.com/ohler55/slip/pkg/test"

	// Pull in CSV functions.
	_ "github.com/ohler55/slip/pkg/csv"

	// Pull in XML functions.
	_ "github.com/ohler55/slip/pkg/xml"

	// Pull in watch functions.
	_ "github.com/ohler55/slip/pkg/watch"

	// Pull in swank server for SLIME integration.
	_ "github.com/ohler55/slip/pkg/swank"

	// Pull in slynk server for SLY integration.
	_ "github.com/ohler55/slip/pkg/slynk"

	// Pull in alive server for VSCode Alive extension.
	_ "github.com/ohler55/slip/pkg/alive"
)
