// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"testing"

	"github.com/ohler55/ojg"
	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestVanillaMethods(t *testing.T) {
	code := slip.ReadString(`
(defflavor strawberry ((size "medium")) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size))
(defflavor blackberry ((size "medium") (fresh nil)) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size fresh))
(setq berry (make-instance 'strawberry :size "medium"))
`)
	scope := slip.NewScope()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'strawberry)").Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'blackberry)").Eval(scope, nil)

	id := slip.ReadString("(send berry :id)").Eval(scope, nil)
	tt.SameType(t, slip.Fixnum(0), id)

	f := slip.ReadString("(send berry :flavor)").Eval(scope, nil)
	tt.Equal(t, "#<flavor strawberry>", f.String())

	has := slip.ReadString("(send berry :operation-handled-p :size)").Eval(scope, nil)
	tt.Equal(t, slip.True, has)
	has = slip.ReadString("(send berry :operation-handled-p :x)").Eval(scope, nil)
	tt.Nil(t, has)
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :operation-handled-p)").Eval(scope, nil) })

	methods := slip.ReadString("(send berry :which-operations)").Eval(scope, nil)
	tt.Equal(t,
		"(:change-class :change-flavor :describe :equal :eval-inside-yourself :flavor :id :init :inspect "+
			":operation-handled-p\n               :print-self :send-if-handles :set-size :shared-initialize "+
			":size :update-instance-for-different-class\n               :which-operations)",
		methods.String())

	pr, pw, err := os.Pipe()
	tt.Nil(t, err)
	ansi := scope.Get("*print-ansi*")
	orig := slip.StandardOutput
	slip.StandardOutput = (*slip.FileStream)(pw)
	defer func() { _ = pw.Close(); _ = pr.Close(); slip.StandardOutput = orig; scope.Set("*print-ansi*", ansi) }()

	scope.Set("*print-ansi*", nil)

	_ = slip.ReadString("(send berry :describe)").Eval(scope, nil)

	pw.Close()
	var out []byte
	out, err = ioutil.ReadAll(pr)
	tt.Nil(t, err)
	tt.Equal(t, `/#<strawberry [0-9a-f]+>, an instance of flavor strawberry,
  has instance variable values:
    size: "medium"
/`, string(out))

	pr2, pw2, err2 := os.Pipe()
	tt.Nil(t, err2)
	defer func() { _ = pw2.Close(); _ = pr2.Close() }()

	scope.Let(slip.Symbol("out"), (*slip.FileStream)(pw2))
	_ = slip.ReadString("(send berry :print-self out 0 t)").Eval(scope, nil)
	pw2.Close()
	out, err = ioutil.ReadAll(pr2)
	tt.Nil(t, err)
	tt.Equal(t, "/#<strawberry [0-9a-f]+>/", string(out))

	tt.Panic(t, func() { _ = slip.ReadString("(send berry :print-self out 0)").Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :print-self t 0 t)").Eval(scope, nil) })
	// try to write to a closed stream
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :print-self out 0 t)").Eval(scope, nil) })

	size := slip.ReadString("(send berry :send-if-handles :size)").Eval(scope, nil)
	tt.Equal(t, slip.String("medium"), size)
	result := slip.ReadString("(send berry :send-if-handles :nothing)").Eval(scope, nil)
	tt.Nil(t, result)
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :send-if-handles)").Eval(scope, nil) })

	size = slip.ReadString("(send berry :eval-inside-yourself 'size)").Eval(scope, nil)
	tt.Equal(t, slip.String("medium"), size)
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :eval-inside-yourself)").Eval(scope, nil) })

	bag := slip.ReadString("(send berry :inspect)").Eval(scope, nil)
	tt.SameType(t, &flavors.Instance{}, bag)
	inst := bag.(*flavors.Instance)
	tt.Equal(t, `/{"flavor":"strawberry","id":"[0-9a-fA-F]+","vars":{"size":"medium"}}/`,
		oj.JSON(inst.Any, &ojg.Options{Sort: true, Indent: 0}))

	_ = slip.ReadString(`(send berry :shared-initialize '(size) :size "large")`).Eval(scope, nil)
	result = slip.ReadString("(send berry :size)").Eval(scope, nil)
	tt.Equal(t, slip.String("large"), result)

	berry := slip.ReadString("berry").Eval(scope, nil).(*flavors.Instance)
	dup := berry.Dup()
	scope.Let(slip.Symbol("dup"), dup)
	berry.ChangeFlavor(flavors.Find("blackberry"))
	_ = slip.ReadString(`(send berry :update-instance-for-different-class dup :fresh t)`).Eval(scope, nil)

	fresh := slip.ReadString(`(send (send berry :inspect) :get "vars.fresh")`).Eval(scope, nil)
	tt.Equal(t, slip.True, fresh)

	tt.Panic(t, func() {
		_ = slip.ReadString(`(send berry :update-instance-for-different-class dup :quux t)`).Eval(scope, nil)
	})
}

func TestVanillaEqualAny(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-bag "{a:1}") :equal (make-bag "{a:1}"))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(send (make-bag "{a:1}") :equal (make-bag "{b:1}"))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-bag "{a:1}") :equal)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-bag "{a:1}") :equal 1 2)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestVanillaDescribeDocs(t *testing.T) {
	testVanillaDocs(t, ":describe",
		`:describe is a method of vanilla-flavor
  vanilla-flavor :primary
    :describe &optional output-stream
     output-stream [output-stream] to write to. (default: *standard-output*)
`+"   "+`
    Writes a description of the instance to output-stream.
`)
}

func TestVanillaEvalSelfDocs(t *testing.T) {
	testVanillaDocs(t, ":eval-inside-yourself",
		`:eval-inside-yourself is a method of vanilla-flavor
  vanilla-flavor :primary
    :eval-inside-yourself form => object
     form [object] to evaluate in the scope of the instance.
`+"   "+`
    Evaluates the form in the instance's scope.
`)
}

func TestVanillaFlavorDocs(t *testing.T) {
	testVanillaDocs(t, ":flavor",
		`:flavor is a method of vanilla-flavor
  vanilla-flavor :primary
    :flavor => flavor
`+"   "+`
    Returns the flavor of the instance.
`)
}

func TestVanillaIDDocs(t *testing.T) {
	testVanillaDocs(t, ":id",
		`:id is a method of vanilla-flavor
  vanilla-flavor :primary
    :id => string
`+"   "+`
    Returns the identifier of the instance.
`)
}

func TestVanillaInitDocs(t *testing.T) {
	testVanillaDocs(t, ":init",
		`:init is a method of vanilla-flavor
  vanilla-flavor :primary
    Does nothing but is a placeholder for daemons in sub-flavors.
`)
}

func TestVanillaInspectDocs(t *testing.T) {
	testVanillaDocs(t, ":inspect",
		`:inspect is a method of vanilla-flavor
  vanilla-flavor :primary
    :inspect => bag
`+"   "+`
    Returns a bag with the details of the instance.
`)
}

func TestVanillaOpHandledDocs(t *testing.T) {
	testVanillaDocs(t, ":operation-handled-p",
		`:operation-handled-p is a method of vanilla-flavor
  vanilla-flavor :primary
    :operation-handled-p method => boolean
     method [keyword] to check.
`+"   "+`
    Returns t if the instance handles the method and nil otherwise.
`)
}

func TestVanillaPrintSelfDocs(t *testing.T) {
	testVanillaDocs(t, ":print-self",
		`:print-self is a method of vanilla-flavor
  vanilla-flavor :primary
    :print-self &optional stream &rest ignored
     stream [output-stream] to write the description to. The default is *standard-output*
`+"   "+`
    Writes a description of the instance to the stream.
`)
}

func TestVanillaSendIfDocs(t *testing.T) {
	testVanillaDocs(t, ":send-if-handles",
		`:send-if-handles is a method of vanilla-flavor
  vanilla-flavor :primary
    :send-if-handles method arguments* => object
     method [keyword] to send to the instance if the instance has the method.
     arguments* to pass to the method call.
`+"   "+`
    Sends to the instance if the instance has the method.
`)
}

func TestVanillaChangeClassDocs(t *testing.T) {
	testVanillaDocs(t, ":change-class",
		`:change-class is a method of vanilla-flavor
  vanilla-flavor :primary
    :change-class new-class &key &allow-other-keys) => instance
      new-class [flavor] the new flavor for the instance.
`+"   "+`
    Returns self after changing the flavor of the instance. When called a copy of the instance is created and the
    :update-instance-for-different-class is called on the original after the flavor has been changed to the new flavor.
    The previous is a copy of the original instance. The original instance has already been changed and the slots
    adjusted for the new flavor. This validates the keywords and then calls the :shared-initialize method.
`)
}

func TestVanillaSharedInitializeDocs(t *testing.T) {
	testVanillaDocs(t, ":shared-initialize",
		`:shared-initialize is a method of vanilla-flavor
  vanilla-flavor :primary
    :shared-initialize slot-names &rest initargs &key &allow-other-keys) => instance
      slot-names [list] a list of the slot names in the re-flavored instance.
      initargs [list] additional arguments are ignored by the default method.
`+"   "+`
    Returns self after processing the key arguments to set the slots in the instance.
`)
}

func TestVanillaUpdateInstanceForDifferentClassDocs(t *testing.T) {
	testVanillaDocs(t, ":update-instance-for-different-class",
		`:update-instance-for-different-class is a method of vanilla-flavor
  vanilla-flavor :primary
    :update-instance-for-different-class previous &rest initargs &key &allow-other-keys)
      previous [instance] a copy of the original instance.
      initargs [list] additional arguments are ignored by the default method.
`+"   "+`
    When change-class is called a copy of the instance is created and the :update-instance-for-different-class is called
    on the original after the flavor has been changed to the new flavor. The previous is a copy of the original
    instance. The original instance has already been changed and the slots adjusted for the new flavor. This validates
    the keywords and then calls the :shared-initialize method.
`)
}

func TestVanillaWhichOpsDocs(t *testing.T) {
	testVanillaDocs(t, ":which-operations",
		`:which-operations is a method of vanilla-flavor
  vanilla-flavor :primary
    :which-operations => list
`+"   "+`
    Returns a list of all the methods the instance handles.
`)
}

func TestVanillaEqualDocs(t *testing.T) {
	testVanillaDocs(t, ":equal",
		`:equal is a method of vanilla-flavor
  vanilla-flavor :primary
    :equal other => boolean
      other [object] other object to compare to self.
`+"   "+`
    Returns t if the instance is of the same flavor as other and has the same content.
`)
}

func testVanillaDocs(t *testing.T, method, expect string) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*print-ansi*"), nil)
	_ = slip.ReadString(fmt.Sprintf("(describe-method vanilla-flavor %s out)", method)).Eval(scope, nil)
	tt.Equal(t, expect, out.String())
}

func TestVanillaDescribeStream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(setq obj (make-instance 'vanilla-flavor))").Eval(scope, nil)
	_ = slip.ReadString("(send obj :describe out)").Eval(scope, nil)
	tt.Equal(t, `/#<vanilla-flavor [0-9a-f]+>.*, an instance of .*vanilla-flavor.*/`, out.String())

	tt.Panic(t, func() { _ = slip.ReadString("(send obj :describe t)").Eval(scope, nil) })
}
