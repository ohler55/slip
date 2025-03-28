// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// VectorLike is an interface that all vectors implement.
type VectorLike interface {
	ArrayLike

	// Adjust array with new parameters.
	Adjust(dims []int, eType Symbol, initVal Object, initContent List, fillPtr int) VectorLike
}
