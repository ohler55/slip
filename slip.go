// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

func init() {
	ConstantDocs["*features*"] = "Features of the implementation."
	addFeature("slip")
}

func addFeature(name string) {
	features, _ := ConstantValues["*features*"].(List)
	ConstantValues["*features*"] = append(features, Symbol(":"+name))
}
