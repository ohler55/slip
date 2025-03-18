// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// ArrayLike is an interface that all array and vectors implement.
type ArrayLike interface {
	Object

	// ArrayType returns the array type.
	ArrayType() Symbol

	// Dimensions of the array.
	Dimensions() []int

	// Size of the array.
	Size() int

	// AsList the Object into set of nested lists.
	AsList() List

	// Rank of the array is returned,
	Rank() int

	// Adjustable returns true if the array is adjustable.
	Adjustable() bool

	// ElementType returns the element-type of the array.
	ElementType() Symbol
}
