// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// VectorLike is an interface that all vectors implement.
type VectorLike interface {
	ArrayLike

	// FillPointer returns the fill-pointer as an int.
	FillPointer() int

	// SetFillPointer sets the fill-pointer.
	SetFillPointer(int)

	// Adjust array with new parameters.
	Adjust(dims []int, eType Symbol, initVal Object, initContent List, fillPtr int) VectorLike
}
