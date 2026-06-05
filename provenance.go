// Copyright (c) 2026, Peter Ohler, All rights reserved.

package slip

// Prov repreesnts function provenance which include the filepath and location
// in the file of a function.
type Prov struct {
	Filepath    string
	FirstLine   uint32
	LastLine    uint32
	FirstColumn uint16
	LastColumn  uint16
	Count       uint32 // doubles as the index in the code reader starts slice
}
