// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

func init() {
	DefConstant(Symbol("*features*"), List{Symbol(":slip")}, `Features of the implementation.`)
}
