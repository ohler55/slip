// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("$/alive/listPackages", handleListPackages)
}

// handleListPackages handles the $/alive/listPackages request.
// Returns a list of package names and their nicknames.
func handleListPackages(c *Connection, params interface{}) (interface{}, error) {
	packages := slip.AllPackages()

	result := make([]map[string]interface{}, 0, len(packages))
	for _, pkg := range packages {
		pkgInfo := map[string]interface{}{
			"name": pkg.Name,
		}
		if len(pkg.Nicknames) > 0 {
			pkgInfo["nicknames"] = pkg.Nicknames
		}
		result = append(result, pkgInfo)
	}

	return result, nil
}
