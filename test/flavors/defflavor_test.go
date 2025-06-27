// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pp"
)

func TestDefflavorBasic(t *testing.T) {
	defer undefFlavor("strawberry")

	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor strawberry ((size "medium")) ()
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables
 (:documentation "Strawberry icecream"))
`, scope)
	tt.Equal(t, slip.Symbol("strawberry"), code.Eval(scope, nil))

	f := slip.ReadString("strawberry", scope).Eval(scope, nil)
	sf := f.Simplify()
	tt.Equal(t, "strawberry", jp.C("name").First(sf))
	tt.Equal(t, "Strawberry icecream", jp.C("docs").First(sf))

	daemons := jp.MustParseString("methods[?(@.name == ':size')]").Get(sf)
	tt.Equal(t, 1, len(daemons))

	daemons = jp.MustParseString("methods[?(@.name == ':set-size')]").Get(sf)
	tt.Equal(t, 1, len(daemons))

	tt.Equal(t, `
(defflavor strawberry ((size "medium"))
                      ()
  :inittable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables
  (:documentation "Strawberry icecream"))
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorInittable(t *testing.T) {
	defer undefFlavor("strawberry")

	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor strawberry (size color) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size))
`, scope)
	tt.Equal(t, slip.Symbol("strawberry"), code.Eval(scope, nil))

	f := slip.ReadString("strawberry", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor strawberry (color size)
                      ()
  (:inittable-instance-variables size)
  :gettable-instance-variables
  :settable-instance-variables)
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorGettableSettable(t *testing.T) {
	defer undefFlavor("strawberry")

	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor strawberry ((size "medium") color) ()
 (:gettable-instance-variables size)
 (:settable-instance-variables size))
`, scope)
	tt.Equal(t, slip.Symbol("strawberry"), code.Eval(scope, nil))

	f := slip.ReadString("strawberry", scope).Eval(scope, nil)
	sf := f.Simplify()

	daemons := jp.MustParseString("methods[?(@.name == ':size')]").Get(sf)
	tt.Equal(t, 1, len(daemons))

	daemons = jp.MustParseString("methods[?(@.name == ':color')]").Get(sf)
	tt.Equal(t, 0, len(daemons))

	daemons = jp.MustParseString("methods[?(@.name == ':set-size')]").Get(sf)
	tt.Equal(t, 1, len(daemons))

	daemons = jp.MustParseString("methods[?(@.name == ':set-color')]").Get(sf)
	tt.Equal(t, 0, len(daemons))

	tt.Equal(t, `
(defflavor strawberry (color (size "medium"))
                      ()
  (:gettable-instance-variables size)
  (:settable-instance-variables size))
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorInherit(t *testing.T) {
	defer undefFlavors("f1", "f2", "f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) ())
(defflavor f2 ((b 2)) (f1))
(defflavor f3 () (f2))
`, scope).Eval(scope, nil)

	names := slip.ReadString("*all-flavor-names*", scope).Eval(scope, nil)
	tt.Equal(t, "/f1 f2 f3/",
		names.String())

	sf := slip.ReadString("f3", scope).Eval(scope, nil).Simplify()
	tt.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
	tt.Equal(t, "{a: 1 b: 2 self: null}", pretty.SEN(jp.C("defaultVars").First(sf)))

	f := slip.ReadString("f3", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor f3 ()
              (f2 f1))
`, string(pp.Append([]byte{'\n'}, scope, f)))

	slip.ReadString("(undefflavor 'f1)", scope).Eval(scope, nil)

	// undefflavor should remove all flavors that inherit from f1 as well as f1.
	names = slip.ReadString("*all-flavor-names*", scope).Eval(scope, nil)
	tt.Equal(t, false, strings.Contains(names.String(), "f1"))
	tt.Equal(t, false, strings.Contains(names.String(), "f2"))
	tt.Equal(t, false, strings.Contains(names.String(), "f3"))
}

func TestDefflavorInheritSame(t *testing.T) {
	defer undefFlavors("f1", "f2", "f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) ())
(defflavor f2 ((b 2)) (f1))
(defflavor f3 () (f1 f2))
`, scope).Eval(scope, nil)

	sf := slip.ReadString("f3", scope).Eval(scope, nil).Simplify()
	tt.Equal(t, "[f1 f2 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))

	f := slip.ReadString("f3", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor f3 ()
              (f1 f2))
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorNoVanilla(t *testing.T) {
	defer undefFlavors("chocolate")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor chocolate ((a 1)) () :no-vanilla-flavor)
`, scope).Eval(scope, nil)

	sf := slip.ReadString("chocolate", scope).Eval(scope, nil).Simplify()
	tt.Equal(t, "[]", pretty.SEN(jp.C("inherit").First(sf)))

	f := slip.ReadString("chocolate", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor chocolate ((a 1))
                     ()
  :no-vanilla-flavor)
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorKeywords(t *testing.T) {
	defer undefFlavors("f1")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((b 2)) () (:init-keywords :x :y) (:required-init-keywords :x))
`, scope).Eval(scope, nil)

	sf := slip.ReadString("f1", scope).Eval(scope, nil).Simplify()
	tt.Equal(t, `[":x"]`, pretty.SEN(jp.C("requiredKeywords").First(sf)))
	tt.Equal(t, `{":x": null ":y": null}`, pretty.SEN(jp.C("keywords").First(sf)))

	f := slip.ReadString("f1", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor f1 ((b 2))
              ()
  (:default-init-plist (:x nil) (:y nil))
  (:required-init-keywords :x))
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorInitPlist(t *testing.T) {
	defer undefFlavors("f1")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((b 2)) () (:default-init-plist (:allow-other-keys t) (:x 1) (:y 2)))
`, scope).Eval(scope, nil)

	sf := slip.ReadString("f1", scope).Eval(scope, nil).Simplify()
	tt.Equal(t, `{":x": 1 ":y": 2}`, pretty.SEN(jp.C("keywords").First(sf)))

	f := slip.ReadString("f1", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor f1 ((b 2))
              ()
  (:default-init-plist (:allow-other-keys t) (:x 1) (:y 2)))
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorInitPlistInherit(t *testing.T) {
	defer undefFlavors("f1")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) () (:default-init-plist (:allow-other-keys t) (:x 1) (:y 2)))
(defflavor f2 ((b 2)) (f1))
`, scope).Eval(scope, nil)

	sf := slip.ReadString("f2", scope).Eval(scope, nil).Simplify()
	tt.Equal(t, `{":x": 1 ":y": 2}`, pretty.SEN(jp.C("keywords").First(sf)))

	f := slip.ReadString("f2", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor f2 ((b 2))
              (f1)
  (:default-init-plist (:allow-other-keys t) (:x 1) (:y 2)))
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorInitBadPlist(t *testing.T) {
	defer undefFlavors("f1")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f1 ((b 2)) () (:default-init-plist (t 2)))
`, scope).Eval(scope, nil)
	})
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f1 ((b 2)) () (:default-init-plist :x))
`, scope).Eval(scope, nil)
	})
}

func TestDefflavorBadKeywords(t *testing.T) {
	defer undefFlavors("f1")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f1 ((b 2)) () (:init-keywords t))
`, scope).Eval(scope, nil)
	})
}

func TestDefflavorBadGettable(t *testing.T) {
	defer undefFlavors("f1")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f1 ((b 2)) () (:gettable-instance-variables t))
`, scope).Eval(scope, nil)
	})
}

func TestDefflavorBadSettable(t *testing.T) {
	defer undefFlavors("f1")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f1 ((b 2)) () (:settable-instance-variables t))
`, scope).Eval(scope, nil)
	})
}

func TestDefflavorInclude(t *testing.T) {
	defer undefFlavors("f1", "f2", "f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) ())
(defflavor f2 ((b 2)) (f1))
(defflavor f3 () () (:included-flavors f2 f1))
`, scope).Eval(scope, nil)

	names := slip.ReadString("*all-flavor-names*", scope).Eval(scope, nil)
	tt.Equal(t, "/f1 f2 f3/", names.String())

	sf := slip.ReadString("f3", scope).Eval(scope, nil).Simplify()
	tt.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
	tt.Equal(t, "{a: 1 b: 2 self: null}", pretty.SEN(jp.C("defaultVars").First(sf)))

	f := slip.ReadString("f3", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor f3 ()
              (f2 f1)
  (:included-flavors f2 f1))
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorIncludeAbstract(t *testing.T) {
	defer undefFlavors("f1", "f2", "f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) ())
(defflavor f2 ((b 2)) () :abstract-flavor (:included-flavors f1))
(defflavor f3 () () (:included-flavors f2))
`, scope).Eval(scope, nil)

	names := slip.ReadString("*all-flavor-names*", scope).Eval(scope, nil)
	tt.Equal(t, "/f1 f2 f3/", names.String())

	sf := slip.ReadString("f3", scope).Eval(scope, nil).Simplify()
	tt.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
	tt.Equal(t, "{a: 1 b: 2 self: null}", pretty.SEN(jp.C("defaultVars").First(sf)))

	f := slip.ReadString("f2", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor f2 ((b 2))
              ()
  :abstract-flavor
  (:included-flavors f1))
`, string(pp.Append([]byte{'\n'}, scope, f)))

	f = slip.ReadString("f3", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor f3 ()
              (f2 f1)
  (:included-flavors f2))
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorIncludeBadAbstract(t *testing.T) {
	defer undefFlavors("f2", "f3")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f2 ((b 2)) () :abstract-flavor (:included-flavors f4))
(defflavor f3 () () (:included-flavors f2))
`, scope).Eval(scope, nil)
	})
	slip.ReadString("(undefflavor 'f2)", scope).Eval(scope, nil)
}

func TestDefflavorBadArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 ())", scope).Eval(scope, nil) })
}

func TestDefflavorBadName(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor t () ())", scope).Eval(scope, nil) })
}

func TestDefflavorBadDefaultHandler(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:default-handler t))", scope).Eval(scope, nil) })
}

func TestDefflavorShortOption(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:default-handler))", scope).Eval(scope, nil) })
}

func TestDefflavorTooManyOption(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:default-handler bad bad))", scope).Eval(scope, nil) })
}

func TestDefflavorBadOptionKey(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (t t))", scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () :not-an-option)", scope).Eval(scope, nil) })
}

func TestDefflavorBadOption(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () t)", scope).Eval(scope, nil) })
}

func TestDefflavorBadVars(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 t ())", scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 (t) ())", scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 (x (y)) ())", scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 ((1 2)) ())", scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 ((x nil) 1) ())", scope).Eval(scope, nil) })
}

func TestDefflavorBadInheritedFlavor(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () (t))", scope).Eval(scope, nil) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () t)", scope).Eval(scope, nil) })
}

func TestDefflavorDuplicate(t *testing.T) {
	scope := slip.NewScope()
	defer undefFlavor("f1")
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () ()) (defflavor f1 () ())", scope).Eval(scope, nil) })
}

func TestDefflavorInheritNotDefined(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () (bad))", scope).Eval(scope, nil) })
}

func TestDefflavorIncludedNotDefined(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:included-flavors bad))", scope).Eval(scope, nil) })
}

func TestDefflavorRequiredNotSymbol(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:required-flavors t))", scope).Eval(scope, nil) })
}

func TestDefflavorBadDocs(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:documentation t))", scope).Eval(scope, nil) })
}

func TestDefflavorBadInitable(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t,
		func() {
			slip.ReadString("(defflavor f1 () () (:initable-instance-variables t))", scope).Eval(scope, nil)
		})
}

func TestDefflavorRequired(t *testing.T) {
	defer undefFlavors("f1", "f2", "f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) () :gettable-instance-variables)
(defflavor f2 ((b 2)) (f1))
(defflavor f3 () (f2) (:required-flavors f1) (:required-methods :a) (:required-instance-variables b))
`, scope).Eval(scope, nil)

	names := slip.ReadString("*all-flavor-names*", scope).Eval(scope, nil)
	tt.Equal(t, "/f1 f2 f3/", names.String())

	sf := slip.ReadString("f3", scope).Eval(scope, nil).Simplify()
	tt.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))

	f := slip.ReadString("f3", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor f3 ()
              (f2 f1)
  (:required-instance-variables b)
  (:required-methods :a)
  (:required-flavors f1))
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorMissing(t *testing.T) {
	defer undefFlavors("f3")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f3 () () (:required-flavors f1))
`, scope).Eval(scope, nil)
	})
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f3 () () (:required-methods :a))
`, scope).Eval(scope, nil)
	})
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f3 () () (:required-instance-variables b))
`, scope).Eval(scope, nil)
	})
}

func TestDefflavorDefaultHandlerLambda(t *testing.T) {
	defer undefFlavors("f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f3 () () (:default-handler (lambda (&rest args) args)))
`, scope).Eval(scope, nil)

	f := slip.ReadString("f3", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor f3 ()
              ()
  (:default-handler (lambda (&rest args)
                      args)))
`, string(pp.Append([]byte{'\n'}, scope, f)))
}

func TestDefflavorDefaultHandlerFunc(t *testing.T) {
	defer undefFlavors("f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defun anything (&rest args) args)
(defflavor f3 () () (:default-handler anything))
`, scope).Eval(scope, nil)

	f := slip.ReadString("f3", scope).Eval(scope, nil)
	tt.Equal(t, `
(defflavor f3 ()
              ()
  (:default-handler anything))
`, string(pp.Append([]byte{'\n'}, scope, f)))
}
