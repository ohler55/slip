// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"io"
	"os"
	"os/user"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pp"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Snapshot{Function: slip.Function{Name: "snapshot", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "snapshot",
			Args: []*slip.DocArg{
				{
					Name: "destination",
					Type: "output-stream|string|t|nil",
					Text: `The destination to write to. If _t_ then write to _*standard-output*_.
If _nil_ then return a string. If a string then destination is taken as a file name and file will
be either created or over-written`,
				},
			},
			Return: "string|nil",
			Text:   `__snapshot__ TBD limitation as well as behavior`,
			Examples: []string{
				`(snapshot t)`,
			},
		}, &Pkg)
}

// Snapshot represents the snapshot function.
type Snapshot struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Snapshot) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)

	b := AppendSnapshot(nil, s)
	a0 := args[0]
top:
	switch ta := a0.(type) {
	case nil:
		return slip.String(b)
	case io.Writer:
		if _, err := ta.Write(b); err != nil {
			ss, ok := ta.(slip.Stream)
			if !ok {
				ss = &slip.OutputStream{Writer: ta}
			}
			slip.PanicStream(ss, "writing snapshot: %s", err)
		}
	case slip.String:
		f, err := os.Create(string(ta))
		if err != nil {
			slip.PanicFile(ta, "failed to create: %s", err)
		}
		defer func() { _ = f.Close() }()
		if _, err = f.Write(b); err != nil {
			slip.PanicFile(ta, "failed to write: %s", err)
		}
	default:
		if ta == slip.True {
			a0 = s.Get("*standard-output*")
			goto top
		}
		slip.PanicType("destination", ta, "output-stream", "t", "nil", "string")
	}
	return nil
}

// AppendSnapshot appends a snapshot to a []byte.
func AppendSnapshot(b []byte, s *slip.Scope) []byte {
	b = append(b, ";;;; Snapshot taken at "...)
	b = time.Now().AppendFormat(b, time.RFC3339)
	if usr, _ := user.Current(); usr != nil && 0 < len(usr.Username) {
		b = append(b, " by "...)
		b = append(b, usr.Username...)
	}
	b = append(b, '\n')

	b = appendSnapshotRequires(b, s)
	b = appendSnapshotPackages(b, s)
	b = appendSnapshotConstants(b, s)
	b = appendSnapshotFunctions(b, s)
	b = appendSnapshotFlavors(b, s)
	b = appendSnapshotVars(b, s)

	return b
}

func appendSnapshotRequires(b []byte, s *slip.Scope) []byte {
	// Packages with a load path were loaded with a call to require.
	var imported []*slip.Package
	for _, p := range slip.AllPackages() {
		if path := p.LoadPath(); 0 < len(path) {
			imported = append(imported, p)
		}
	}
	if 0 < len(imported) {
		b = append(b, '\n')
		sort.Slice(imported, func(i, j int) bool {
			return imported[i].Name < imported[j].Name
		})
		for _, p := range imported {
			path := p.LoadPath()
			form := slip.List{
				slip.Symbol("require"),
				slip.List{slip.Symbol("quote"), slip.Symbol(strings.TrimSuffix(filepath.Base(path), ".so"))},
				slip.String(filepath.Dir(path)),
			}
			b = pp.Append(b, s, form)
		}
	}
	return b
}

func appendSnapshotPackages(b []byte, s *slip.Scope) []byte {
	var defs []*slip.Package
	for _, p := range slip.AllPackages() {
		if !p.Locked &&
			len(p.LoadPath()) == 0 &&
			p.Name != "flavors" && // flavors is special
			p.Name != "common-lisp-user" &&
			p.Name != "keyword" {
			defs = append(defs, p)
		}
	}
	if 0 < len(defs) {
		b = append(b, '\n')
		sort.Slice(defs, func(i, j int) bool {
			return defs[i].Name < defs[j].Name
		})
		for _, p := range defs {
			form := slip.List{slip.Symbol("defpackage"), slip.String(p.Name)}
			if 0 < len(p.Doc) {
				form = append(form, slip.List{slip.Symbol(":documentation"), slip.String(p.Doc)})
			}
			if 0 < len(p.Nicknames) {
				nn := slip.List{slip.Symbol(":nicknames")}
				for _, n := range p.Nicknames {
					nn = append(nn, slip.String(n))
				}
				form = append(form, nn)
			}
			if 0 < len(p.Uses) {
				ul := slip.List{slip.Symbol(":use")}
				for _, u := range p.Uses {
					ul = append(ul, slip.String(u.Name))
				}
				form = append(form, ul)
			}
			if 0 < len(p.Exports) {
				xl := slip.List{slip.Symbol(":export")}
				for _, x := range p.Exports {
					xl = append(xl, slip.String(x))
				}
				form = append(form, xl)
			}
			b = pp.Append(b, s, form)
		}
	}
	return b
}

func appendSnapshotConstants(b []byte, s *slip.Scope) []byte {
	// Skip locked and imported packages.
	var ca []*slip.Constant
	for _, c := range slip.Constants {
		if c.Pkg != nil && !c.Pkg.Locked && len(c.Pkg.LoadPath()) == 0 && c.Pkg.Name != "flavors" {
			ca = append(ca, c)
		}
	}
	if 0 < len(ca) {
		b = append(b, '\n')
		sort.Slice(ca, func(i, j int) bool {
			if ca[i].Pkg.Name == ca[j].Pkg.Name {
				return ca[i].Name < ca[j].Name
			}
			return ca[i].Pkg.Name < ca[j].Pkg.Name
		})
		for _, c := range ca {
			form := slip.List{
				slip.Symbol("defconstant"),
				slip.Symbol(strings.Join([]string{c.Pkg.Name, c.Name}, "::")),
				c.Value,
			}
			if 0 < len(c.Doc) {
				form = append(form, slip.String(c.Doc))
			}
			b = pp.Append(b, s, form)
		}
	}
	return b
}

func appendSnapshotFunctions(b []byte, s *slip.Scope) []byte {

	return b
}

func appendSnapshotFlavors(b []byte, s *slip.Scope) []byte {

	return b
}

func appendSnapshotVars(b []byte, s *slip.Scope) []byte {
	// vars for each package, just use user if var is in user - defvar then setq

	return b
}
