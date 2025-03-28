// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// ArrayLike is an interface that all array and vectors implement.
type ArrayLike interface {
	Object

	// ArrayType returns the array type.
	ArrayType() Symbol

	// Dimensions of the array.
	Dimensions() []int

	// Length returns the length of the object.
	Length() int

	// AsList the Object into set of nested lists.
	AsList() List

	// Rank of the array is returned,
	Rank() int

	// Adjustable returns true if the array is adjustable.
	Adjustable() bool

	// ElementType returns the element-type of the array.
	ElementType() Symbol

	// Get the value at the location identified by the indexes.
	Get(indexes ...int) Object

	// Set a value at the location identified by the indexes.
	Set(value Object, indexes ...int)

	// MajorIndex for the indexes provided.
	MajorIndex(indexes ...int) int

	// MajorGet for the index provided.
	MajorGet(index int) Object

	// MajorSet for the index provided.
	MajorSet(index int, value Object)
}
