// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"os"
	"os/exec"
	"path/filepath"

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
			"scratch":        nil,
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
 - A :scratch and :cache variable are included.


The usual use of a __system__ instance is to first send the instance a :fetch
method to cache sources and then invoke one of the operations defined in the
:in-order-to variable.

`),
			},
			slip.List{
				slip.Symbol(":default-handler"),
				// TBD a function to parse and run the target in :in-order-to
				// define a function that given a :in-order-to spec invokes the specified code
				//  additional argument are assigned somehow, maybe with keys?
				slip.Symbol("car"),
			},
		},
		&Pkg,
	)
	system.DefMethod(":fetch", "", systemFetchCaller{})
	system.DefMethod(":load", "", systemLoadCaller{})
	system.DefMethod(":run", "", systemRunCaller{})

	system.Document("components", "An ordered list of the files to load for the system.")
	system.Document("scratch", "Filepath to a scratch area.")
	system.Document("cache", "Filepath to the import cache.")
	system.Document("depends-on", `The sources this system depends on. The entries listed are
imported into the system cache. The elements of the :depends-on are lists that start with a
source name then a type keyword and are followed by a lambda list. The supported source
keywords with lambda list descriptions are:
   __:file__ root filepath*
   __:git__ url &key branch tag commit sub-dir
   __:system__ filepath
   __:require__ package-name load-path
   __:call__ function
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
	sources, ok := self.Get("depends-on").(slip.List)
	if !ok {
		slip.PanicType("depends-on", self.Get("depends-on"), "list")
	}
	if 0 < len(sources) {
		cache := getStringVar(self, "cache", "cache")
		if err := os.MkdirAll(cache, 0755); err != nil {
			panic(err)
		}
		for _, src := range sources {
			var ll slip.List
			if ll, ok = src.(slip.List); !ok || len(ll) < 3 {
				slip.PanicType("source", src, "list")
			}
			dir := filepath.Join(cache, slip.MustBeString(ll[0], "source name"))
			if err := os.MkdirAll(dir, 0755); err != nil {
				panic(err)
			}
			switch ll[1] {
			case slip.Symbol(":file"):
				fetchFiles(self, dir, ll[2:], cache)
			case slip.Symbol(":git"):
				fetchGit(self, dir, ll[2:], cache)
			case slip.Symbol(":system"):
				fetchSystem(self, dir, ll[2:], cache)
			case slip.Symbol(":require"):
				fetchRequire(self, dir, ll[2:], cache)
			case slip.Symbol(":call"):
				fetchCall(self, dir, ll[2:], cache)
			default:
				slip.NewPanic("%s is not a valid source type.", ll[0])
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
	// TBD
	return nil
}

func (caller systemLoadCaller) Docs() string {
	return `__:load__

Loads all sources specified in the the :depends-on variable.
`
}

type systemRunCaller struct{}

func (caller systemRunCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// TBD
	return nil
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

func fetchFiles(self *flavors.Instance, dir string, args slip.List, cache string) {
	root := slip.MustBeString(args[0], "root")
	for _, arg := range args[1:] {
		path := slip.MustBeString(arg, "filepath")
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
		_ = os.RemoveAll(dest)
		if err := exec.Command("cp", "-r", src, dest).Run(); err != nil {
			slip.NewPanic("Failed to copy %s to %s. %s\n", src, dest, err)
		}
	}
}

func fetchGit(self *flavors.Instance, dir string, args slip.List, cache string) {
	gu := slip.MustBeString(args[0], "url")
	rest := args[1:]
	scratch := getStringVar(self, "scratch", ".scratch")
	var subdir string
	if val, has := slip.GetArgsKeyValue(rest, slip.Symbol(":sub-dir")); has {
		subdir = slip.MustBeString(val, "sub-dir")
	}
	if val, has := slip.GetArgsKeyValue(rest, slip.Symbol(":tag")); has {
		tag := slip.MustBeString(val, "tag")
		fetchGitTag(self, dir, gu, tag, subdir, scratch, cache)
		return
	}
	if val, has := slip.GetArgsKeyValue(rest, slip.Symbol(":branch")); has {
		branch := slip.MustBeString(val, "branch")
		fetchGitBranch(self, dir, gu, branch, subdir, scratch, cache)
		return
	}
	if val, has := slip.GetArgsKeyValue(rest, slip.Symbol(":commit")); has {
		commit := slip.MustBeString(val, "commit")
		fetchGitCommit(self, dir, gu, commit, subdir, scratch, cache)
		return
	}
	slip.NewPanic("A git source must specify a tag, branch, or commit.")
}

func fetchGitTag(self *flavors.Instance, dir, gitURL, tag, subdir, scratch, cache string) {
	_ = os.RemoveAll(scratch)
	if err := exec.Command("git", "clone", "--depth=1", gitURL, scratch).Run(); err != nil {
		slip.NewPanic("Failed to git clone %s to %s. %s\n", gitURL, scratch, err)
	}
	if err := exec.Command("git", "-C", scratch, "checkout", "tags/"+tag).Run(); err != nil {
		slip.NewPanic("Failed to git checkout tag %s. %s\n", tag, err)
	}
	mvGitScratchToCache(dir, scratch, subdir, cache)
}

func fetchGitBranch(self *flavors.Instance, dir, gitURL, branch, subdir, scratch, cache string) {
	_ = os.RemoveAll(scratch)
	if err := exec.Command("git", "clone", "-b", branch, "--depth=1", gitURL, scratch).Run(); err != nil {
		slip.NewPanic("Failed to git clone %s to %s. %s\n", gitURL, scratch, err)
	}
	mvGitScratchToCache(dir, scratch, subdir, cache)
}

func fetchGitCommit(self *flavors.Instance, dir, gitURL, commit, subdir, scratch, cache string) {
	_ = os.RemoveAll(scratch)
	if err := exec.Command("git", "clone", "--depth=1", gitURL, scratch).Run(); err != nil {
		slip.NewPanic("Failed to git clone %s to %s. %s\n", gitURL, scratch, err)
	}
	if err := exec.Command("git", "-C", scratch, "checkout", commit).Run(); err != nil {
		slip.NewPanic("Failed to git checkout commit %s. %s\n", commit, err)
	}
	mvGitScratchToCache(dir, scratch, subdir, cache)
}

func fetchSystem(self *flavors.Instance, dir string, args slip.List, cache string) {

}

func fetchRequire(self *flavors.Instance, dir string, args slip.List, cache string) {

}

func fetchCall(self *flavors.Instance, dir string, args slip.List, cache string) {

}

func mvGitScratchToCache(dir, scratch, subdir, cache string) {
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
