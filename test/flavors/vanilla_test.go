// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"fmt"
	"io"
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
	scope := slip.NewScope()
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
`, scope)
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'strawberry)", scope).Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'blackberry)", scope).Eval(scope, nil)

	id := slip.ReadString("(send berry :id)", scope).Eval(scope, nil)
	tt.SameType(t, slip.Fixnum(0), id)

	f := slip.ReadString("(send berry :flavor)", scope).Eval(scope, nil)
	tt.Equal(t, "#<flavor strawberry>", f.String())

	has := slip.ReadString("(send berry :operation-handled-p :size)", scope).Eval(scope, nil)
	tt.Equal(t, slip.True, has)
	has = slip.ReadString("(send berry :operation-handled-p :x)", scope).Eval(scope, nil)
	tt.Nil(t, has)
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :operation-handled-p)", scope).Eval(scope, nil) })

	methods := slip.ReadString("(send berry :which-operations)", scope).Eval(scope, nil)
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

	_ = slip.ReadString("(send berry :describe)", scope).Eval(scope, nil)

	_ = pw.Close()
	var out []byte
	out, err = io.ReadAll(pr)
	tt.Nil(t, err)
	tt.Equal(t, `/#<strawberry [0-9a-f]+>, an instance of flavor strawberry,
  has instance variable values:
    size: "medium"
/`, string(out))

	pr2, pw2, err2 := os.Pipe()
	tt.Nil(t, err2)
	defer func() { _ = pw2.Close(); _ = pr2.Close() }()

	scope.Let(slip.Symbol("out"), (*slip.FileStream)(pw2))
	_ = slip.ReadString("(send berry :print-self out 0 t)", scope).Eval(scope, nil)
	_ = pw2.Close()
	out, err = io.ReadAll(pr2)
	tt.Nil(t, err)
	tt.Equal(t, "/#<strawberry [0-9a-f]+>/", string(out))

	tt.Panic(t, func() { _ = slip.ReadString("(send berry :print-self out 0)", scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :print-self t 0 t)", scope).Eval(scope, nil) })
	// try to write to a closed stream
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :print-self out 0 t)", scope).Eval(scope, nil) })

	size := slip.ReadString("(send berry :send-if-handles :size)", scope).Eval(scope, nil)
	tt.Equal(t, slip.String("medium"), size)
	result := slip.ReadString("(send berry :send-if-handles :nothing)", scope).Eval(scope, nil)
	tt.Nil(t, result)
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :send-if-handles)", scope).Eval(scope, nil) })

	size = slip.ReadString("(send berry :eval-inside-yourself 'size)", scope).Eval(scope, nil)
	tt.Equal(t, slip.String("medium"), size)
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :eval-inside-yourself)", scope).Eval(scope, nil) })

	bag := slip.ReadString("(send berry :inspect)", scope).Eval(scope, nil)
	tt.SameType(t, &flavors.Instance{}, bag)
	inst := bag.(*flavors.Instance)
	tt.Equal(t, `/{"flavor":"strawberry","id":"[0-9a-fA-F]+","vars":{"size":"medium"}}/`,
		oj.JSON(inst.Any, &ojg.Options{Sort: true, Indent: 0}))

	_ = slip.ReadString(`(send berry :shared-initialize '(size) :size "large")`, scope).Eval(scope, nil)
	result = slip.ReadString("(send berry :size)", scope).Eval(scope, nil)
	tt.Equal(t, slip.String("large"), result)

	berry := slip.ReadString("berry", scope).Eval(scope, nil).(*flavors.Instance)
	dup := berry.Dup()
	scope.Let(slip.Symbol("dup"), dup)
	berry.ChangeFlavor(flavors.Find("blackberry"))
	_ = slip.ReadString(`(send berry :update-instance-for-different-class dup :fresh t)`, scope).Eval(scope, nil)

	fresh := slip.ReadString(`(send (send berry :inspect) :get "vars.fresh")`, scope).Eval(scope, nil)
	tt.Equal(t, slip.True, fresh)

	tt.Panic(t, func() {
		_ = slip.ReadString(`(send berry :update-instance-for-different-class dup :quux t)`, scope).Eval(scope, nil)
	})

	_ = slip.ReadString(`(send berry :change-class 'strawberry :size "small")`, scope).Eval(scope, nil)
	result = slip.ReadString(`(send (send berry :inspect) :get "vars.size")`, scope).Eval(scope, nil)
	tt.Equal(t, slip.String("small"), result)
	result = slip.ReadString(`(send (send berry :inspect) :get "flavor")`, scope).Eval(scope, nil)
	tt.Equal(t, slip.String("strawberry"), result)

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(change-class berry 'none-berry)`,
		PanicType: slip.ClassNotFoundSymbol,
	}).Test(t)
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
  Lambda-List: (&optional (output-stream *standard-output*))
  Documentation:
    Writes a description of the instance to output-stream.
  Arguments:
    output-stream: [output-stream] = *standard-output*
      output-stream to write to.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaEvalSelfDocs(t *testing.T) {
	testVanillaDocs(t, ":eval-inside-yourself",
		`:eval-inside-yourself is a method of vanilla-flavor
  Lambda-List: (form)
  Return: object
  Documentation:
    Evaluates the form in the instance's scope.
  Arguments:
    form: [object]
      Form to evaluate in the scope of the instance.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaFlavorDocs(t *testing.T) {
	testVanillaDocs(t, ":flavor",
		`:flavor is a method of vanilla-flavor
  Lambda-List: ()
  Return: flavor
  Documentation:
    Returns the flavor of the instance.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaIDDocs(t *testing.T) {
	testVanillaDocs(t, ":id",
		`:id is a method of vanilla-flavor
  Lambda-List: ()
  Return: fixnum
  Documentation:
    Returns the identifier of the instance.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaInitDocs(t *testing.T) {
	testVanillaDocs(t, ":init",
		`:init is a method of vanilla-flavor
  Lambda-List: ()
  Documentation:
    Does nothing but is a placeholder for daemons in sub-flavors.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaInspectDocs(t *testing.T) {
	testVanillaDocs(t, ":inspect",
		`:inspect is a method of vanilla-flavor
  Lambda-List: ()
  Return: bag
  Documentation:
    Returns a bag with the details of the instance.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaOpHandledDocs(t *testing.T) {
	testVanillaDocs(t, ":operation-handled-p",
		`:operation-handled-p is a method of vanilla-flavor
  Lambda-List: (method)
  Return: boolean
  Documentation:
    Returns t if the instance handles the method and nil otherwise.
  Arguments:
    method: [keyword]
      Symbol to check.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaPrintSelfDocs(t *testing.T) {
	testVanillaDocs(t, ":print-self",
		`:print-self is a method of vanilla-flavor
  Lambda-List: (&optional (output-stream *standard-output*) &rest rest)
  Documentation:
    Writes a description of the instance to the stream.
  Arguments:
    output-stream: [output-stream] = *standard-output*
      output-stream to write the description to.
    rest: []
      ignored

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaSendIfDocs(t *testing.T) {
	testVanillaDocs(t, ":send-if-handles",
		`:send-if-handles is a method of vanilla-flavor
  Lambda-List: (method &rest arguments*)
  Return: object
  Documentation:
    Sends to the instance if the instance has the method.
  Arguments:
    method: [keyword]
      Method to send to the instance if the instance has the method.
    arguments*: []
      Argument to pass to the method call.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaChangeClassDocs(t *testing.T) {
	testVanillaDocs(t, ":change-class",
		`:change-class is a method of vanilla-flavor
  Lambda-List: (new-class)
  Return: instance
  Documentation:
    Returns self after changing the flavor of the instance. When called a copy of the instance is
    created and the :update-instance-for-different-class is called on the original after the flavor
    has been changed to the new flavor. The previous is a copy of the original instance. The
    original instance has already been changed and the slots adjusted for the new flavor. This
    validates the keywords and then calls the :shared-initialize method.

    This method is an extension of the original flavors.
  Arguments:
    new-class: [flavor]
      The new flavor for the instance.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaSharedInitializeDocs(t *testing.T) {
	testVanillaDocs(t, ":shared-initialize",
		`:shared-initialize is a method of vanilla-flavor
  Lambda-List: (slot-names &rest initargs &key &allow-other-keys)
  Return: instance
  Documentation:
    Returns self after processing the key arguments to set the slots in the instance.

    This method is an extension of the original flavors.
  Arguments:
    slot-names: [list]
      A list of the slot names in the re-flavored instance.
    initargs: [list]
      Additional arguments are ignored by the default method.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaUpdateInstanceForDifferentClassDocs(t *testing.T) {
	testVanillaDocs(t, ":update-instance-for-different-class",
		`:update-instance-for-different-class is a method of vanilla-flavor
  Lambda-List: (previous &rest initargs &key &allow-other-keys)
  Return: instance
  Documentation:
    When change-class is called a copy of the instance is created and the
    :update-instance-for-different-class is called on the original after the flavor has been
    changed to the new flavor. The previous is a copy of the original instance. The original
    instance has already been changed and the slots adjusted for the new flavor. This validates the
    keywords and then calls the :shared-initialize method.

    This method is an extension of the original flavors.
  Arguments:
    previous: [instance]
      a copy of the original instance.
    initargs: [list]
      Additional arguments are ignored by the default method.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaWhichOpsDocs(t *testing.T) {
	testVanillaDocs(t, ":which-operations",
		`:which-operations is a method of vanilla-flavor
  Lambda-List: ()
  Return: list
  Documentation:
    Returns a list of all the methods the instance handles.

  Implemented by:
    vanilla-flavor :primary
`)
}

func TestVanillaEqualDocs(t *testing.T) {
	testVanillaDocs(t, ":equal",
		`:equal is a method of vanilla-flavor
  Lambda-List: (other)
  Return: boolean
  Documentation:
    Returns t if the instance is of the same flavor as other and has the same content.
  Arguments:
    other: [object]
      Other object to compare to self.

  Implemented by:
    vanilla-flavor :primary
`)
}

func testVanillaDocs(t *testing.T, method, expect string) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*print-ansi*"), nil)
	scope.Let(slip.Symbol("*print-right-margin*"), slip.Fixnum(100))
	_ = slip.ReadString(fmt.Sprintf("(describe-method vanilla-flavor %s out)", method), scope).Eval(scope, nil)
	tt.Equal(t, expect, compactEmptyLines(out.String()))
}

func compactEmptyLines(s string) string {
	size := len(s)
	for {
		s = strings.ReplaceAll(s, " \n", "\n")
		if size == len(s) {
			break
		}
		size = len(s)
	}
	return s
}

func TestVanillaDescribeStream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(setq obj (make-instance 'vanilla-flavor))", scope).Eval(scope, nil)
	_ = slip.ReadString("(send obj :describe out)", scope).Eval(scope, nil)
	tt.Equal(t, `/#<vanilla-flavor [0-9a-f]+>.*, an instance of .*vanilla-flavor.*/`, out.String())

	tt.Panic(t, func() { _ = slip.ReadString("(send obj :describe t)", scope).Eval(scope, nil) })
}

func TestChangeClassFlavor(t *testing.T) {
	scope := slip.NewScope()
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
`, scope)
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'strawberry)", scope).Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'blackberry)", scope).Eval(scope, nil)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(change-class berry (find-class 'blackberry))`,
		Expect: "/#<blackberry [0-9a-f]+>/",
	}).Test(t)
}

func TestChangeClassNotInstance(t *testing.T) {
	(&sliptest.Function{
		Source:    `(change-class (make-instance 'vanilla-flavor) 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
