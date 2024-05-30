// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

const (
	// AmpBody is &body.
	AmpBody = "&body"
	// AmpKey is &key.
	AmpKey = "&key"
	// AmpOptional is &optional.
	AmpOptional = "&optional"
	// AmpRest is &rest.
	AmpRest = "&rest"
)

// DocArg describes a function argument.
type DocArg struct {
	Name    string
	Type    string
	Text    string
	Default Object
}

// FuncDoc describes a function.
type FuncDoc struct {
	Name     string
	Args     []*DocArg
	Return   string // return type
	Text     string
	Examples []string
	Kind     Symbol
	NoExport bool
}

func (fd *FuncDoc) getArg(name string) *DocArg {
	for _, a := range fd.Args {
		if a.Name == name {
			return a
		}
	}
	return nil
}
