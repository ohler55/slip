// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"github.com/ohler55/slip"
)

func init() {
	// Package listing
	RegisterHandler("slynk:list-all-packages", handleListAllPackages)

	// Package switching
	RegisterHandler("slynk:set-package", handleSetPackage)

	// Package info
	RegisterHandler("slynk:package-local-nicknames", handlePackageLocalNicknames)

	// Module loading (stub)
	RegisterHandler("slynk:slynk-require", handleSlynkRequire)
}

// handleListAllPackages returns a list of all loaded packages.
func handleListAllPackages(c *Connection, args slip.List) slip.Object {
	var packages slip.List

	for _, pkg := range slip.AllPackages() {
		packages = append(packages, slip.String(pkg.Name))
	}

	return packages
}

// handleSetPackage changes the current package.
// Returns: (package-name prompt-string)
func handleSetPackage(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		// Return current package
		return slip.List{
			slip.String(c.currentPkg.Name),
			slip.String(c.currentPkg.Name),
		}
	}

	pkgName, ok := args[0].(slip.String)
	if !ok {
		return slip.List{
			slip.String(c.currentPkg.Name),
			slip.String(c.currentPkg.Name),
		}
	}

	pkg := slip.FindPackage(string(pkgName))
	if pkg == nil {
		// Package not found, stay in current
		return slip.List{
			slip.String(c.currentPkg.Name),
			slip.String(c.currentPkg.Name),
		}
	}

	c.currentPkg = pkg
	c.defaultChan.SetPackage(pkg)

	// Notify about package change
	c.NewPackage(pkg.Name, pkg.Name)

	return slip.List{
		slip.String(pkg.Name),
		slip.String(pkg.Name),
	}
}

// handlePackageLocalNicknames returns package nickname information.
func handlePackageLocalNicknames(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.List{}
	}

	pkgName, ok := args[0].(slip.String)
	if !ok {
		return slip.List{}
	}

	pkg := slip.FindPackage(string(pkgName))
	if pkg == nil {
		return slip.List{}
	}

	// Build nickname list
	var nicknames slip.List
	for _, nick := range pkg.Nicknames {
		nicknames = append(nicknames, slip.String(nick))
	}

	return nicknames
}

// handleSlynkRequire loads Slynk modules.
// This is a stub that returns success for requested modules.
func handleSlynkRequire(c *Connection, args slip.List) slip.Object {
	// Return nil to indicate success (no additional modules needed to load)
	return nil
}
