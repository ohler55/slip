// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("swank:list-all-package-names", handleListAllPackageNames)
	RegisterHandler("swank:set-package", handleSetPackage)
	RegisterHandler("swank:find-definitions-for-emacs", handleFindDefinitions)
	RegisterHandler("swank:xref", handleXref)
	RegisterHandler("swank:xrefs", handleXrefs)
}

// handleListAllPackageNames returns a list of all package names.
// Response format: (package-name ...)
func handleListAllPackageNames(c *Connection, args slip.List) slip.Object {
	packages := slip.AllPackages()
	result := make(slip.List, 0, len(packages)*2)

	for _, pkg := range packages {
		result = append(result, slip.String(pkg.Name))
		for _, nick := range pkg.Nicknames {
			result = append(result, slip.String(nick))
		}
	}

	return result
}

// handleSetPackage changes the current package.
// Response format: (package-name prompt-string)
func handleSetPackage(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.List{
			slip.String(c.currentPkg.Name),
			slip.String(c.currentPkg.Name),
		}
	}

	name, ok := args[0].(slip.String)
	if !ok {
		return slip.List{
			slip.String(c.currentPkg.Name),
			slip.String(c.currentPkg.Name),
		}
	}

	if pkg := slip.FindPackage(string(name)); pkg != nil {
		c.currentPkg = pkg
		// Notify SLIME of the package change
		c.NewPackage(pkg.Name, pkg.Name)
	}

	return slip.List{
		slip.String(c.currentPkg.Name),
		slip.String(c.currentPkg.Name),
	}
}

// handleFindDefinitions finds the definitions of a symbol.
// Response format: ((label location) ...)
// We don't have source locations, so return empty for now.
func handleFindDefinitions(c *Connection, args slip.List) slip.Object {
	// SLIP doesn't track source locations, so return empty
	return slip.List{}
}

// handleXref finds cross-references for a symbol.
// Response format: ((designator location) ...)
func handleXref(c *Connection, args slip.List) slip.Object {
	// SLIP doesn't track cross-references, so return empty
	return slip.List{}
}

// handleXrefs finds multiple cross-references.
func handleXrefs(c *Connection, args slip.List) slip.Object {
	// SLIP doesn't track cross-references, so return empty
	return slip.List{}
}
