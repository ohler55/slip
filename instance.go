// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

type Instance interface {

	// Class of the instance.
	Class() Class

	// TBD maybe ChangeClass(nc Classy)
}
