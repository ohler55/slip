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
	"github.com/ohler55/slip/pkg/flavors"
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
			Text: `__snapshot__ takes a snapshot of the currently defined constants, variables,
functions, classes, and instances. Objects that can not be encoded such as streams are excluded.
Thw snapshot is written as a LISP file suitable for loading to re-created the current state.`,
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
	slip.CheckArgCount(s, depth, f, args, 1, 1)

	b := AppendSnapshot(nil, s)
	a0 := args[0]
top:
	switch ta := a0.(type) {
	case nil:
		return slip.String(b)
	case io.Writer:
		if _, err := ta.Write(b); err != nil {
			ss, _ := ta.(slip.Stream)
			slip.PanicStream(ss, "writing snapshot: %s", err)
		}
	case slip.String:
		f, err := os.Create(string(ta))
		if err != nil {
			slip.FilePanic(s, depth, ta, "failed to create: %s", err)
		}
		defer func() { _ = f.Close() }()
		a0 = (*slip.FileStream)(f)
		goto top
	default:
		if ta == slip.True {
			a0 = s.Get("*standard-output*")
			goto top
		}
		slip.TypePanic(s, depth, "destination", ta, "output-stream", "t", "nil", "string")
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
	b = appendSnapshotFlavors(b, s)
	b = appendSnapshotVars(b, s)
	b = appendSnapshotFunctions(b, s)

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
	var va []*slip.VarVal
	for _, p := range slip.AllPackages() {
		if !isCorePackage(p) {
			p.EachVarVal(func(name string, vv *slip.VarVal) {
				if p == vv.Pkg && vv.Const && vv.String() != "*common-lisp-user*" {
					va = append(va, vv)
				}
			})
		}
	}
	if 0 < len(va) {
		b = append(b, '\n')
		sort.Slice(va, func(i, j int) bool {
			if va[i].Pkg.Name == va[j].Pkg.Name {
				return va[i].String() < va[j].String()
			}
			return va[i].Pkg.Name < va[j].Pkg.Name
		})
		for _, c := range va {
			form := slip.List{
				slip.Symbol("defconstant"),
				slip.Symbol(strings.Join([]string{c.Pkg.Name, c.String()}, "::")),
				c.Value(),
			}
			if 0 < len(c.Doc) {
				form = append(form, slip.String(c.Doc))
			}
			b = pp.Append(b, s, form)
		}
	}
	return b
}

func appendSnapshotFlavors(b []byte, s *slip.Scope) []byte {
	var fa []*flavors.Flavor
	for _, f := range flavors.All() {
		p := f.Pkg()
		if p != nil && !p.Locked && len(p.LoadPath()) == 0 && p.Name != "flavors" {
			fa = append(fa, f)
		}
	}
	sort.Slice(fa, func(i, j int) bool {
		return fa[j].Inherits(fa[i])
	})
	for _, f := range fa {
		b = append(b, '\n')
		b = pp.Append(b, s, f.LoadForm())
	}
	return b
}

var excludeVars = map[string]bool{
	"*all-flavor-names*": true,
	"*current-test*":     true,
	"*error-output*":     true,
	"*load-truename*":    true,
	"*random-state*":     true,
	"*standard-input*":   true,
	"*standard-output*":  true,
	"*terminal-io*":      true,
	"*trace-output*":     true,
}

func appendSnapshotVars(b []byte, s *slip.Scope) []byte {
	// Skip locked and imported packages.
	var va []*slip.VarVal
	for _, p := range slip.AllPackages() {
		p.EachVarVal(func(name string, vv *slip.VarVal) {
			if p == vv.Pkg && !vv.Const && !excludeVars[name] {
				va = append(va, vv)
			}
		})
	}
	sort.Slice(va, func(i, j int) bool {
		if va[i].Pkg.Name == va[j].Pkg.Name {
			return va[i].String() < va[j].String()
		}
		return va[i].Pkg.Name < va[j].Pkg.Name
	})
	b = append(b, '\n')
	for _, vv := range va {
		if !isCorePackage(vv.Pkg) {
			b = appendDefVar(b, s, vv)
		}
		b = appendSetq(b, s, vv)
	}
	return b
}

func appendDefVar(b []byte, s *slip.Scope, vv *slip.VarVal) (out []byte) {
	form := slip.List{
		slip.Symbol("defvar"),
		slip.Symbol(strings.Join([]string{vv.Pkg.Name, vv.String()}, "::")),
	}
	if 0 < len(vv.Doc) {
		form = append(form, nil)
		form = append(form, slip.String(vv.Doc))
	}
	return pp.Append(b, s, form)
}

func appendSetq(b []byte, s *slip.Scope, vv *slip.VarVal) (out []byte) {
	defer func() {
		if recover() != nil {
			out = b
		}
	}()
	form := slip.List{
		slip.Symbol("setq"),
		slip.Symbol(strings.Join([]string{vv.Pkg.Name, vv.String()}, "::")),
		ppValue(vv.Value()),
	}
	return pp.Append(b, s, form)
}

func ppValue(v slip.Object) (pv slip.Object) {
	pv = v
	switch tv := v.(type) {
	case slip.List:
		if 0 < len(tv) {
			pv = slip.List{slip.Symbol("quote"), tv}
		}
	case *slip.Package:
		pv = slip.List{
			slip.Symbol("find-package"),
			slip.String(tv.Name),
		}
	case *flavors.Flavor:
		pv = slip.List{
			slip.Symbol("find-flavor"),
			slip.String(tv.Name()),
		}
	case *flavors.Instance:
		pv = ppInstance(tv)
	}
	return
}

func ppInstance(inst *flavors.Instance) slip.Object {
	vars := inst.AllVars()
	names := make([]string, 0, len(vars)-1)
	for name := range vars {
		if name != "self" {
			names = append(names, name)
		}
	}
	sort.Strings(names)
	form := slip.List{
		slip.Symbol("let"),
		slip.List{
			slip.List{
				slip.Symbol("inst"),
				slip.List{
					slip.Symbol("make-instance"),
					slip.List{
						slip.Symbol("quote"),
						slip.Symbol(inst.Type.Name()),
					},
				},
			},
		},
	}
	for _, name := range names {
		iv, _ := inst.LocalGet(slip.Symbol(name))
		form = append(form,
			slip.List{
				slip.Symbol("setf"),
				slip.List{
					slip.Symbol("slot-value"),
					slip.Symbol("inst"),
					slip.List{
						slip.Symbol("quote"),
						slip.Symbol(name),
					},
				},
				ppValue(iv),
			},
		)
	}
	form = append(form, slip.Symbol("inst"))

	return form
}

func appendSnapshotFunctions(b []byte, s *slip.Scope) []byte {
	// Skip locked and imported packages.
	for _, p := range slip.AllPackages() {
		if isCorePackage(p) {
			continue
		}
		var fia []*slip.FuncInfo
		p.EachFuncInfo(func(fi *slip.FuncInfo) {
			if fi.Pkg == p {
				fia = append(fia, fi)
			}
		})
		if 0 < len(fia) {
			sort.Slice(fia, func(i, j int) bool {
				return fia[i].Name < fia[j].Name
			})
			b = append(b, '\n')
			b = pp.Append(b, s, slip.List{
				slip.Symbol("use-package"),
				slip.String(p.Name),
			})
			for _, fi := range fia {
				b = append(b, '\n')
				b = pp.Append(b, s, fi)
			}
		}
	}
	b = append(b, '\n')
	b = pp.Append(b, s, slip.List{
		slip.Symbol("use-package"),
		slip.String(slip.CurrentPackage.Name),
	})
	return b
}

func isCorePackage(p *slip.Package) bool {
	return p.Locked ||
		0 < len(p.LoadPath()) ||
		p.Name == "flavors" ||
		p.Name == "keyword"
}
