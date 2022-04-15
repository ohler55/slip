// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/slip"

	"github.com/stretchr/testify/require"
)

func TestPackage(t *testing.T) {
	pkgs := slip.PackageNames()
	require.Equal(t, `("COMMON-LISP" "COMMON-LISP-USER")`, pkgs.String())
}

func TestPackageVars(t *testing.T) {

	fmt.Printf("*** %s\n", pretty.SEN(&slip.UserPkg))
}
