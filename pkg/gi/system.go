// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"plugin"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var system *flavors.Flavor

func defSystem() {
	Pkg.Initialize(nil)
	system = flavors.DefFlavor(
		"system",
		map[string]slip.Object{
			"name":           nil,
			"author":         nil,
			"maintainer":     nil,
			"license":        nil,
			"version":        nil,
			"homepage":       nil,
			"bug-tracker":    nil,
			"source-control": nil,
			"description":    nil,
			"depends-on":     nil,
			"components":     nil,
			"in-order-to":    nil,
			"cache":          nil,
		},
		nil, // inherit
		slip.List{ // options
			slip.Symbol(":inittable-instance-variables"),
			slip.Symbol(":gettable-instance-variables"),
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`Instances of this Flavor define a system similar to ASDF in common LISP
(https://asdf.common-lisp.dev) but with some differences. A __system__ instance
captures the information associated with a code that implements the
system. This includes providence variables such as author, version, and source
location to name a few.

Some of the differences when compared to ASDF are:

 - Not all depends-on sources need be in a common directory.
 - An import cache is used to keep local copies of source systems.
 - Support for using git repositories is included.
 - The :components variable only allows for files and not modules.
 - The :depends-on variable differs to support git and other system sources.
 - The :in-order-to variable differs although it serves the same purpose.
 - The __system__ is a Flavor that also supports a :fetch method.
 - A :cache variable is included.


The usual use of a __system__ instance is to first send the instance a :fetch
method to cache sources and then invoke one of the operations defined in the
:in-order-to variable.

`),
			},
		},
		&Pkg,
	)
	system.DefMethod(":fetch", "", systemFetchCaller{})
	system.DefMethod(":load", "", systemLoadCaller{})
	system.DefMethod(":run", "", systemRunCaller{})

	system.Document("components", "An ordered list of the files to load for the system.")
	system.Document("cache", "Filepath to the import cache.")
	system.Document("depends-on", `The sources this system depends on. The entries listed are
imported into the system cache. The elements of the :depends-on are lists that start with a
source name then a type keyword and are followed by a lambda list. The supported source
keywords with lambda list descriptions are:
   __:file__ root &key files system
   __:git__ url &key branch tag commit sub-dir scratch files system
   __:require__ package-name load-path
   __:call__ fetch-function load-function
`)
	system.Document("in-order-to", `The system can perform operations once the code has been
loaded. Those operations are described in the :in-order-to variable which is a list of lists.
Each list element starts with an operation name such as :test and is followed by the function
to invoke to implement that operation. When system :run is called with key values those keys
are bound to the values and are available to the function being called.
`)
}

type systemFetchCaller struct{}

func (caller systemFetchCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	var sources slip.List
	switch tdo := self.Get("depends-on").(type) {
	case nil:
	case slip.List:
		sources = tdo
	default:
		slip.PanicType("depends-on", self.Get("depends-on"), "list")
	}
	if 0 < len(sources) {
		cache := getStringVar(self, "cache", "cache")
		if err := os.MkdirAll(cache, 0755); err != nil {
			panic(err)
		}
		for _, src := range sources {
			ll, ok := src.(slip.List)
			if !ok || len(ll) < 3 {
				slip.PanicType("source", src, "list")
			}
			dir := filepath.Join(cache, slip.MustBeString(ll[0], "source name"))
			_ = os.RemoveAll(dir)
			_ = os.MkdirAll(dir, 0755) // ignore error and let later failures catch it
			switch ll[1] {
			case slip.Symbol(":file"):
				fetchFiles(self, dir, ll[2:])
			case slip.Symbol(":git"):
				fetchGit(self, dir, ll[2:])
			case slip.Symbol(":require"):
				fetchRequire(self, dir, ll[2:])
			case slip.Symbol(":call"):
				fetchCall(self, dir, ll[2:])
			default:
				slip.NewPanic("%s is not a valid source type.", ll[1])
			}
		}
	}
	return nil
}

func (caller systemFetchCaller) Docs() string {
	return `__:fetch__

Fetches all sources specified in the the :depends-on variable and places them
in the cache.
`
}

type systemLoadCaller struct{}

func (caller systemLoadCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	var sources slip.List
	switch tdo := self.Get("depends-on").(type) {
	case nil:
	case slip.List:
		sources = tdo
	default:
		slip.PanicType("depends-on", self.Get("depends-on"), "list")
	}
	cache := getStringVar(self, "cache", "cache")
	for _, src := range sources {
		ll, ok := src.(slip.List)
		if !ok || len(ll) < 3 {
			slip.PanicType("source", src, "list")
		}
		dir := filepath.Join(cache, slip.MustBeString(ll[0], "source name"))
		switch ll[1] {
		case slip.Symbol(":file"), slip.Symbol(":git"):
			loadFiles(self, dir, ll[2:])
		case slip.Symbol(":require"):
			loadRequire(self, dir, ll[2:])
		case slip.Symbol(":call"):
			loadCall(self, dir, ll[2:])
		default:
			slip.NewPanic("%s is not a valid source type.", ll[1])
		}
	}
	gc := self.Get("components")
	if gc != nil {
		var components slip.List
		components, ok := gc.(slip.List)
		if !ok {
			slip.PanicType("components", gc, "list")
		}
		for _, comp := range components {
			var path slip.String
			if path, ok = comp.(slip.String); !ok {
				slip.PanicType("component", comp, "string")
			}
			matches, _ := filepath.Glob(string(path))
			if len(matches) == 0 {
				matches, _ = filepath.Glob(string(path) + ".lisp")
				if len(matches) == 0 {
					slip.NewPanic("%s not found.", path)
				}
			}
			for _, m := range matches {
				loadFile(self, m)
			}
		}
	}
	return nil
}

func (caller systemLoadCaller) Docs() string {
	return `__:load__

Loads all sources specified in the the :depends-on variable.
`
}

type systemRunCaller struct{}

func (caller systemRunCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.ArgCountCheck(self, args, 1, -1)
	iot, ok := self.Get("in-order-to").(slip.List)
	if !ok {
		slip.PanicType("in-order-to", self.Get("in-order-to"), "list")
	}
	var op slip.Object
	for _, v := range iot {
		var list slip.List
		if list, ok = v.(slip.List); ok && 1 < len(list) && list[0] == args[0] {
			op = list[1]
			break
		}
	}
	scope := self.NewScope()
	for i := 1; i < len(args); i += 2 {
		var key slip.Symbol
		if key, ok = args[i].(slip.Symbol); !ok || len(key) < 2 || key[0] != ':' {
			slip.PanicType("in-order-to key/values", args[i], "keyword")
		}
		if i+1 < len(args) {
			scope.Set(key[1:], args[i+1])
		} else {
			scope.Set(key[1:], nil)
		}
	}
	switch to := op.(type) {
	case nil:
		// nothing to do
	case slip.List:
		result = slip.CompileList(to).Eval(scope, 0)
	default:
		result = to.Eval(scope, 0)
	}
	return
}

func (caller systemRunCaller) Docs() string {
	return `__:run__ _op_ &key _*_
   _op_ [symbol] names the operation of one of the :in-order-to elements.
   _:*_ [symbol&value] keys to bind to the values and made available to the operation.


Run an operation from the :in-order-to variable with the provided keys bound to the values.
`
}

func getStringVar(self *flavors.Instance, key, defVal string) (sval string) {
	val := self.Get(slip.Symbol(key))
	switch tv := val.(type) {
	case slip.String:
		sval = string(tv)
	case nil:
		sval = defVal
	default:
		slip.PanicType(key, tv, "string")
	}
	return
}

func fetchFiles(self *flavors.Instance, dir string, args slip.List) {
	root := slip.MustBeString(args[0], "root")
	if val, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":files")); has {
		files, ok := val.(slip.List)
		if !ok {
			slip.PanicType(":files", val, "list")
		}
		for _, v := range files {
			path := slip.MustBeString(v, "file")
			src := filepath.Join(root, path)
			dest := filepath.Join(dir, path)
			_, err := os.Stat(src)
			if err != nil {
				path += ".lisp"
				src = filepath.Join(root, path)
				dest = filepath.Join(dir, path)
				if _, err = os.Stat(src); err != nil {
					slip.NewPanic("%s not found.", src)
				}
			}
			if err := exec.Command("cp", "-r", src, dest).Run(); err != nil {
				slip.NewPanic("Failed to copy %s to %s. %s\n", src, dest, err)
			}
		}
		return
	}
	if 1 < len(root) && root[len(root)-1] != '/' {
		root = root + "/"
	}
	if err := exec.Command("cp", "-r", root, dir).Run(); err != nil {
		slip.NewPanic("Failed to copy %s to %s. %s\n", root, dir, err)
	}
}

func fetchGit(self *flavors.Instance, dir string, args slip.List) {
	gu := slip.MustBeString(args[0], "url")
	rest := args[1:]
	scratch := ".scratch"
	if val, has := slip.GetArgsKeyValue(rest, slip.Symbol(":scratch")); has {
		scratch = slip.MustBeString(val, "scratch")
	}
	var subdir string
	if val, has := slip.GetArgsKeyValue(rest, slip.Symbol(":sub-dir")); has {
		subdir = slip.MustBeString(val, "sub-dir")
	}
	if val, has := slip.GetArgsKeyValue(rest, slip.Symbol(":tag")); has {
		tag := slip.MustBeString(val, "tag")
		fetchGitTag(self, dir, gu, tag, subdir, scratch)
		return
	}
	if val, has := slip.GetArgsKeyValue(rest, slip.Symbol(":branch")); has {
		branch := slip.MustBeString(val, "branch")
		fetchGitBranch(self, dir, gu, branch, subdir, scratch)
		return
	}
	if val, has := slip.GetArgsKeyValue(rest, slip.Symbol(":commit")); has {
		commit := slip.MustBeString(val, "commit")
		fetchGitCommit(self, dir, gu, commit, subdir, scratch)
		return
	}
	slip.NewPanic("A git source must specify a tag, branch, or commit.")
}

func fetchGitTag(self *flavors.Instance, dir, gitURL, tag, subdir, scratch string) {
	_ = os.RemoveAll(scratch)
	if err := exec.Command("git", "clone", "--depth=1", gitURL, scratch).Run(); err != nil {
		slip.NewPanic("Failed to git clone %s to %s. %s\n", gitURL, scratch, err)
	}
	if err := exec.Command("git", "-C", scratch, "checkout", "tags/"+tag).Run(); err != nil {
		slip.NewPanic("Failed to git checkout tag %s. %s\n", tag, err)
	}
	mvGitScratchToCache(dir, scratch, subdir)
}

func fetchGitBranch(self *flavors.Instance, dir, gitURL, branch, subdir, scratch string) {
	_ = os.RemoveAll(scratch)
	if err := exec.Command("git", "clone", "-b", branch, "--depth=1", gitURL, scratch).Run(); err != nil {
		slip.NewPanic("Failed to git clone %s to %s. %s\n", gitURL, scratch, err)
	}
	mvGitScratchToCache(dir, scratch, subdir)
}

func fetchGitCommit(self *flavors.Instance, dir, gitURL, commit, subdir, scratch string) {
	_ = os.RemoveAll(scratch)
	if err := exec.Command("git", "clone", gitURL, scratch).Run(); err != nil {
		slip.NewPanic("Failed to git clone %s to %s. %s\n", gitURL, scratch, err)
	}
	if err := exec.Command("git", "-C", scratch, "checkout", commit).Run(); err != nil {
		slip.NewPanic("Failed to git checkout commit %s. %s\n", commit, err)
	}
	mvGitScratchToCache(dir, scratch, subdir)
}

func fetchRequire(self *flavors.Instance, dir string, args slip.List) {
	slip.ArgCountCheck(self, args, 2, 2)
	path := filepath.Join(slip.MustBeString(args[1], "load-path"), slip.MustBeString(args[0], "package-name"))
	if err := exec.Command("cp", path, dir).Run(); err != nil {
		slip.NewPanic("Failed to copy %s to %s. %s\n", path, dir, err)
	}
}

func fetchCall(self *flavors.Instance, dir string, args slip.List) {
	switch ta := args[0].(type) {
	case nil:
		// nothing to do
	case slip.List:
		scope := self.NewScope()
		scope.Set("cache-dir", slip.String(dir))
		_ = slip.CompileList(ta).Eval(scope, 0)
	default:
		slip.PanicType("fetch-function", args, "list")
	}
}

func loadFiles(self *flavors.Instance, dir string, args slip.List) {
	if val, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":system")); has {
		subsys := slip.MustBeString(val, ":system")
		loadSystemFile(self, dir, filepath.Join(dir, subsys))
		return
	}
	if val, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":files")); has {
		files, ok := val.(slip.List)
		if !ok {
			slip.PanicType(":files", val, "list")
		}
		for _, arg := range files {
			path := slip.MustBeString(arg, "filepath")
			src := filepath.Join(dir, path)

			if fi, err := os.Stat(src); err != nil {
				path += ".lisp"
				src = filepath.Join(dir, path)
				if _, err = os.Stat(src); err != nil {
					slip.NewPanic("%s not found.", src)
				}
			} else if fi.IsDir() {
				loadDir(self, src)
				continue
			}
			loadFile(self, src)
		}
		return
	}
	loadDir(self, dir)
}

func loadDir(self *flavors.Instance, dir string) {
	_ = filepath.Walk(dir, func(path string, fi fs.FileInfo, err error) error {
		if fi != nil && fi.Mode().IsRegular() && strings.HasSuffix(path, ".lisp") {
			loadFile(self, path)
		}
		return nil
	})
}

func loadSystemFile(self *flavors.Instance, dir, path string) {
	sys, ok := loadFile(self, path).(slip.Instance)
	if !ok {
		slip.PanicType("system", sys, "instance")
	}
	wd, _ := os.Getwd()
	defer func() { _ = os.Chdir(wd) }()
	_ = os.Chdir(dir)
	scope := self.NewScope()
	sys.Receive(scope, ":fetch", nil, 0)
	sys.Receive(scope, ":load", nil, 0)
}

func loadRequire(self *flavors.Instance, dir string, args slip.List) {
	slip.ArgCountCheck(self, args, 2, 2)
	path := filepath.Join(dir, slip.MustBeString(args[0], "package-name"))
	if _, err := plugin.Open(path); err != nil {
		slip.NewPanic("plugin %s open failed. %s", path, err)
	}
}

func loadCall(self *flavors.Instance, dir string, args slip.List) {
	switch ta := args[1].(type) {
	case nil:
		// nothing to do
	case slip.List:
		scope := self.NewScope()
		scope.Set("cache-dir", slip.String(dir))
		_ = slip.CompileList(ta).Eval(scope, 0)
	default:
		slip.PanicType("load-function", args, "list")
	}
}

func loadFile(self *flavors.Instance, path string) (result slip.Object) {
	defer func() {
		self.Set(slip.Symbol("*load-pathname*"), nil)
		self.Set(slip.Symbol("*load-truename*"), nil)
	}()
	self.Set(slip.Symbol("*load-pathname*"), slip.String(path))
	self.Set(slip.Symbol("*load-truename*"), slip.String(path))
	if buf, err := os.ReadFile(path); err == nil {
		code := slip.Read(buf)
		code.Compile()
		result = code.Eval(&self.Scope, nil)
	} else {
		panic(err)
	}
	return
}

func mvGitScratchToCache(dir, scratch, subdir string) {
	// Remove any files at the destination.
	_ = os.RemoveAll(dir)
	src := scratch
	if 0 < len(subdir) {
		src = filepath.Join(src, subdir)
	}
	if err := exec.Command("mv", src, dir).Run(); err != nil {
		slip.NewPanic("Failed to move %s to %s. %s\n", src, dir, err)
	}
	_ = os.RemoveAll(scratch) // cleanup
}
