// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

var features = VarVal{
	Val:   List{Symbol(":slip")},
	Const: true,
	Doc:   "Features of the implementation.",
}

func addFeature(name string) {
	features.Val = append(features.Val.(List), Symbol(":"+name))
}
