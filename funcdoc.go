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
	// AmpAux is &aux.
	AmpAux = "&aux"
	// AmpAllowOtherKeys is &allow-other-keys
	AmpAllowOtherKeys = "&allow-other-keys"
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

// HasFuncDocs is an interface for objects that have FuncDoc documentation.
type HasFuncDocs interface {
	// FuncDocs returns the documentation for the object.
	FuncDocs() *FuncDoc
}

func (fd *FuncDoc) getArg(name string) *DocArg {
	for _, a := range fd.Args {
		if a.Name == name {
			return a
		}
	}
	return nil
}

// DefList return a argument list for function or lambda args list.
func (fd *FuncDoc) DefList() List {
	dl := make(List, len(fd.Args))
	for i, da := range fd.Args {
		if da.Default == nil {
			dl[i] = Symbol(da.Name)
		} else {
			dl[i] = List{Symbol(da.Name), da.Default}
		}
	}
	return dl
}
