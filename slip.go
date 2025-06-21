// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

func init() {
	Constants["*features*"] = &features
}

var features = Constant{
	Name:  "*features*",
	Doc:   "Features of the implementation.",
	Value: List{Symbol(":slip")},
}

func addFeature(name string) {
	features.Value = append(features.Value.(List), Symbol(":"+name))
}
