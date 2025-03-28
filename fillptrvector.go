// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// FillPtrVector is a vector with a possible fillpointer
type FillPtrVector interface {
	VectorLike

	// FillPointer returns the fill-pointer as an int. A value less than zero
	// indicates no fill-pointer is set.
	FillPointer() int

	// SetFillPointer sets the fill-pointer.
	SetFillPointer(int)

	// Pop a value from the vector. The vector elements are not changed if
	// there is a fill-pointer. If there is no fill pointer then the length of
	// the vector is shortened by one.
	Pop() (element Object)

	// Push a value onto the vector.
	Push(values ...Object) (index int)
}
