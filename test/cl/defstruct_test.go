// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestDefstructBasic(t *testing.T) {
	(&sliptest.Macro{
		Source: `(defstruct pt-basic x y)`,
		Validate: func(t *testing.T, result slip.Object) {
			tt.Equal(t, slip.Symbol("pt-basic"), result)
			// Check that the class was registered
			class := slip.FindClass("pt-basic")
			tt.NotNil(t, class)
			tt.Equal(t, "pt-basic", class.Name())
		},
	}).Test(t)
}

func TestDefstructConstructor(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct pt-ctor x y)
(make-pt-ctor :x 10 :y 20)`,
		Validate: func(t *testing.T, result slip.Object) {
			obj, ok := result.(*cl.StructureObject)
			tt.Equal(t, true, ok)
			tt.Equal(t, "pt-ctor", obj.Type.Name())

			x, _ := obj.SlotValue(slip.Symbol("x"))
			y, _ := obj.SlotValue(slip.Symbol("y"))
			tt.Equal(t, slip.Fixnum(10), x)
			tt.Equal(t, slip.Fixnum(20), y)
		},
	}).Test(t)
}

func TestDefstructPredicate(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct pt-pred x y)
(list (pt-pred-p (make-pt-pred :x 1 :y 2))
      (pt-pred-p 42))`,
		Expect: "(t nil)",
	}).Test(t)
}

func TestDefstructAccessors(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct pt-acc x y)
(let ((p (make-pt-acc :x 5 :y 10)))
  (list (pt-acc-x p) (pt-acc-y p)))`,
		Expect: "(5 10)",
	}).Test(t)
}

func TestDefstructSetf(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct pt-setf x y)
(let ((p (make-pt-setf :x 1 :y 2)))
  (setf (pt-setf-x p) 100)
  (pt-setf-x p))`,
		Expect: "100",
	}).Test(t)
}

func TestDefstructCopier(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct pt-copy x y)
(let* ((p1 (make-pt-copy :x 1 :y 2))
       (p2 (copy-pt-copy p1)))
  (setf (pt-copy-x p2) 999)
  (list (pt-copy-x p1) (pt-copy-x p2)))`,
		Expect: "(1 999)",
	}).Test(t)
}

func TestDefstructInitform(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct counter (value 0))
(counter-value (make-counter))`,
		Expect: "0",
	}).Test(t)
}

func TestDefstructConcName(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (person (:conc-name p-)) name age)
(let ((joe (make-person :name "Joe" :age 30)))
  (list (p-name joe) (p-age joe)))`,
		Expect: `("Joe" 30)`,
	}).Test(t)
}

func TestDefstructConcNameNil(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (thing (:conc-name nil)) value)
(let ((t1 (make-thing :value 42)))
  (value t1))`,
		Expect: "42",
	}).Test(t)
}

func TestDefstructCustomConstructor(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (pt-custom (:constructor create-pt-custom)) x y)
(let ((p (create-pt-custom :x 5 :y 6)))
  (list (pt-custom-x p) (pt-custom-y p)))`,
		Expect: "(5 6)",
	}).Test(t)
}

func TestDefstructNoCopier(t *testing.T) {
	(&sliptest.Macro{
		Source: `(defstruct (nocopier-point (:copier nil)) x y)`,
		Validate: func(t *testing.T, result slip.Object) {
			tt.Equal(t, slip.Symbol("nocopier-point"), result)
			// copy-nocopier-point should not exist
			fn := slip.FindFunc("copy-nocopier-point")
			tt.Equal(t, (*slip.FuncInfo)(nil), fn)
		},
	}).Test(t)
}

func TestDefstructNoPredicate(t *testing.T) {
	(&sliptest.Macro{
		Source: `(defstruct (blob (:predicate nil)) data)`,
		Validate: func(t *testing.T, result slip.Object) {
			tt.Equal(t, slip.Symbol("blob"), result)
			// blob-p should not exist
			fn := slip.FindFunc("blob-p")
			tt.Equal(t, (*slip.FuncInfo)(nil), fn)
		},
	}).Test(t)
}

func TestDefstructInclude(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct point2d x y)
(defstruct (point3d (:include point2d)) z)
(let ((p (make-point3d :x 1 :y 2 :z 3)))
  (list (point3d-x p) (point3d-y p) (point3d-z p)))`,
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestDefstructIncludeTypep(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct base-shape)
(defstruct (rectangle (:include base-shape)) width height)
(let ((r (make-rectangle :width 10 :height 20)))
  (list (rectangle-p r) (base-shape-p r)))`,
		Expect: "(t t)",
	}).Test(t)
}

func TestDefstructReadOnly(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct immutable (id 0 :read-only t))
(let ((obj (make-immutable :id 42)))
  (setf (immutable-id obj) 999))`,
		Panics: true, // Expect panic on setf of read-only slot
	}).Test(t)
}

func TestDefstructDocumentation(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct doc-point
  "A 2D point structure"
  x y)`,
		Validate: func(t *testing.T, result slip.Object) {
			tt.Equal(t, slip.Symbol("doc-point"), result)
			class := slip.FindClass("doc-point")
			tt.Equal(t, "A 2D point structure", class.Documentation())
		},
	}).Test(t)
}

func TestDefstructTypeVector(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vec-point (:type vector) (:named)) x y)
(let ((p (make-vec-point :x 5 :y 10)))
  (list (vec-point-x p) (vec-point-y p) (vectorp p)))`,
		Expect: "(5 10 t)",
	}).Test(t)
}

func TestDefstructTypeVectorNamed(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (named-vec (:type vector) (:named)) a b)
(let ((v (make-named-vec :a 1 :b 2)))
  (svref v 0))`,
		Expect: "named-vec",
	}).Test(t)
}

func TestDefstructTypeList(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (list-point (:type list) (:named)) x y)
(let ((p (make-list-point :x 3 :y 4)))
  (list (list-point-x p) (list-point-y p) (listp p)))`,
		Expect: "(3 4 t)",
	}).Test(t)
}

func TestDefstructBOAConstructor(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (pt-boa (:constructor make-ptboa (x y)))
  x y)
(let ((p (make-ptboa 7 8)))
  (list (pt-boa-x p) (pt-boa-y p)))`,
		Expect: "(7 8)",
	}).Test(t)
}

func TestDefstructBOAOptional(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (pt-boaopt (:constructor make-ptboaopt (x &optional (y 0))))
  x y)
(list (pt-boaopt-y (make-ptboaopt 5)) (pt-boaopt-y (make-ptboaopt 5 10)))`,
		Expect: "(0 10)",
	}).Test(t)
}

func TestDefstructMultipleConstructors(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (pt-multi
             (:constructor make-pt-multi)
             (:constructor make-ptm (x y)))
  x y)
(list
  (pt-multi-x (make-pt-multi :x 1 :y 2))
  (pt-multi-x (make-ptm 3 4)))`,
		Expect: "(1 3)",
	}).Test(t)
}

func TestDefstructPrint(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct simple-point x y)
(princ-to-string (make-simple-point :x 1 :y 2))`,
		Validate: func(t *testing.T, result slip.Object) {
			// Should print in #S(...) format
			str, ok := result.(slip.String)
			tt.Equal(t, true, ok)
			tt.Equal(t, true, len(str) > 0)
		},
	}).Test(t)
}

// Additional tests for coverage

func TestDefstructTypedVectorPredicate(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (tvec (:type vector) (:named)) x)
(list (tvec-p (make-tvec :x 1)) (tvec-p #(not-tvec 1)))`,
		Expect: "(t nil)",
	}).Test(t)
}

func TestDefstructTypedListPredicate(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (tlist (:type list) (:named)) x)
(list (tlist-p (make-tlist :x 1)) (tlist-p '(not-tlist 1)))`,
		Expect: "(t nil)",
	}).Test(t)
}

func TestDefstructTypedVectorCopier(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vcopy (:type vector) (:named)) a b)
(let* ((v1 (make-vcopy :a 1 :b 2))
       (v2 (copy-vcopy v1)))
  (setf (vcopy-a v2) 99)
  (list (vcopy-a v1) (vcopy-a v2)))`,
		Expect: "(1 99)",
	}).Test(t)
}

func TestDefstructTypedListCopier(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (lcopy (:type list) (:named)) a b)
(let* ((l1 (make-lcopy :a 1 :b 2))
       (l2 (copy-lcopy l1)))
  (list (lcopy-a l1) (lcopy-a l2)))`,
		Expect: "(1 1)",
	}).Test(t)
}

func TestDefstructTypedVectorSetf(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vsetf (:type vector) (:named)) x)
(let ((v (make-vsetf :x 10)))
  (setf (vsetf-x v) 20)
  (vsetf-x v))`,
		Expect: "20",
	}).Test(t)
}

func TestDefstructTypedBOAConstructor(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vboa (:type vector) (:named) (:constructor make-vboa (x y))) x y)
(let ((v (make-vboa 5 6)))
  (list (vboa-x v) (vboa-y v)))`,
		Expect: "(5 6)",
	}).Test(t)
}

func TestDefstructTypedBOAOptional(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vboa2 (:type vector) (:named) (:constructor make-vboa2 (x &optional (y 99)))) x y)
(list (vboa2-y (make-vboa2 1)) (vboa2-y (make-vboa2 1 2)))`,
		Expect: "(99 2)",
	}).Test(t)
}

func TestDefstructTypedListBOA(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (lboa (:type list) (:named) (:constructor make-lboa (a b))) a b)
(let ((l (make-lboa 7 8)))
  (list (lboa-a l) (lboa-b l)))`,
		Expect: "(7 8)",
	}).Test(t)
}

func TestDefstructBOAKey(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (kpoint (:constructor make-kpt (&key x y))) x y)
(let ((p (make-kpt :y 20 :x 10)))
  (list (kpoint-x p) (kpoint-y p)))`,
		Expect: "(10 20)",
	}).Test(t)
}

func TestDefstructBOAKeyDefault(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (kdpoint (:constructor make-kdpt (&key (x 1) (y 2)))) x y)
(list (kdpoint-x (make-kdpt)) (kdpoint-y (make-kdpt :y 99)))`,
		Expect: "(1 99)",
	}).Test(t)
}

func TestDefstructBOAAux(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (auxpt (:constructor make-auxpt (x &aux (y 99)))) x y)
(let ((p (make-auxpt 5)))
  (list (auxpt-x p) (auxpt-y p)))`,
		Expect: "(5 99)",
	}).Test(t)
}

func TestDefstructBOARest(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (restpt (:constructor make-restpt (x &rest y))) x y)
(let ((p (make-restpt 1 2 3 4)))
  (list (restpt-x p) (restpt-y p)))`,
		Expect: "(1 (2 3 4))",
	}).Test(t)
}

func TestDefstructCustomPredicate(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (cpred (:predicate is-cpred)) val)
(is-cpred (make-cpred :val 1))`,
		Expect: "t",
	}).Test(t)
}

func TestDefstructCustomCopier(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (ccopy (:copier dup-ccopy)) val)
(ccopy-val (dup-ccopy (make-ccopy :val 42)))`,
		Expect: "42",
	}).Test(t)
}

func TestDefstructNoConstructor(t *testing.T) {
	(&sliptest.Macro{
		Source: `(defstruct (noctor (:constructor nil)) val)`,
		Validate: func(t *testing.T, result slip.Object) {
			tt.Equal(t, slip.Symbol("noctor"), result)
			// make-noctor should not exist
			fn := slip.FindFunc("make-noctor")
			tt.Equal(t, (*slip.FuncInfo)(nil), fn)
		},
	}).Test(t)
}

func TestDefstructSlotType(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct typed-slot (count 0 :type fixnum))
(typed-slot-count (make-typed-slot :count 10))`,
		Expect: "10",
	}).Test(t)
}

func TestDefstructTypeVectorElement(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vecelem (:type (vector t))) a b)
(let ((v (make-vecelem :a 1 :b 2)))
  (list (vecelem-a v) (vecelem-b v)))`,
		Expect: "(1 2)",
	}).Test(t)
}

// ============================================================================
// Direct Go API tests for StructureClass, StructureObject, StructureSlot
// ============================================================================

func TestStructureClassString(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct strtest x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("strtest")
	tt.NotNil(t, sc)
	str := sc.String()
	tt.Equal(t, "#<structure-class strtest>", str)
}

func TestStructureClassAppend(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct apptest x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("apptest")
	b := sc.Append([]byte("prefix:"))
	tt.Equal(t, "prefix:#<structure-class apptest>", string(b))
}

func TestStructureClassSimplify(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct simptest a b)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("simptest")
	simp := sc.Simplify()
	m, ok := simp.(map[string]any)
	tt.Equal(t, true, ok)
	tt.Equal(t, "simptest", m["name"])
}

func TestStructureClassEqual(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct eqtest x) (defstruct eqtest2 x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("eqtest")
	sc2 := cl.FindStructureClass("eqtest")  // Same class, different lookup
	sc3 := cl.FindStructureClass("eqtest2") // Different class
	tt.Equal(t, true, sc.Equal(sc2))
	tt.Equal(t, false, sc.Equal(sc3))
	tt.Equal(t, false, sc.Equal(slip.Fixnum(1)))
}

func TestStructureClassHierarchy(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct hiertest x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("hiertest")
	hier := sc.Hierarchy()
	tt.Equal(t, 3, len(hier))
	tt.Equal(t, slip.Symbol("structure-class"), hier[0])
}

func TestStructureClassEval(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct evaltest x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("evaltest")
	result := sc.Eval(scope, 0)
	tt.Equal(t, sc, result)
}

func TestStructureClassDescribe(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct desctest "test docs" x y)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("desctest")
	b := sc.Describe(nil, 0, 80, false)
	str := string(b)
	tt.Equal(t, true, len(str) > 0)
	// Should contain structure name and slots
	tt.Equal(t, true, contains(str, "desctest"))
	tt.Equal(t, true, contains(str, "test docs"))
}

func TestStructureClassMakeInstance(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct mktest x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("mktest")
	inst := sc.MakeInstance()
	tt.NotNil(t, inst)
	obj, ok := inst.(*cl.StructureObject)
	tt.Equal(t, true, ok)
	tt.Equal(t, sc, obj.Type)
}

func TestStructureClassInheritsList(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`
(defstruct inhbase x)
(defstruct (inhchild (:include inhbase)) y)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("inhchild")
	list := sc.InheritsList()
	tt.Equal(t, 1, len(list))
	tt.Equal(t, "inhbase", list[0].Name())
}

func TestStructureClassLoadForm(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct loadtest (x 0) (y 1 :read-only t))`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("loadtest")
	form := sc.LoadForm()
	tt.NotNil(t, form)
	list, ok := form.(slip.List)
	tt.Equal(t, true, ok)
	tt.Equal(t, slip.Symbol("defstruct"), list[0])
}

func TestStructureClassMetaclass(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct metatest x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("metatest")
	tt.Equal(t, slip.Symbol("structure-class"), sc.Metaclass())
}

func TestStructureClassVarNames(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct vartest b a c)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("vartest")
	names := sc.VarNames()
	tt.Equal(t, 3, len(names))
	// Should be sorted
	tt.Equal(t, "a", names[0])
	tt.Equal(t, "b", names[1])
	tt.Equal(t, "c", names[2])
}

func TestStructureClassSlotCount(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct counttest a b c)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("counttest")
	tt.Equal(t, 3, sc.SlotCount())
}

func TestStructureClassAllSlots(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct alltest a b)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("alltest")
	slots := sc.AllSlots()
	tt.Equal(t, 2, len(slots))
}

func TestStructureClassPrecedence(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct prectest x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("prectest")
	prec := sc.Precedence()
	tt.Equal(t, true, len(prec) >= 3)
	tt.Equal(t, slip.Symbol("prectest"), prec[0])
}

func TestStructureClassGetters(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`
(defstruct (gettest
  (:conc-name gt-)
  (:copier copy-gt)
  (:predicate gt-p)
  (:type vector)
  (:named)) x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("gettest")
	tt.Equal(t, "gt-", sc.ConcName())
	tt.Equal(t, "copy-gt", sc.CopierName())
	tt.Equal(t, "gt-p", sc.PredicateName())
	tt.Equal(t, true, sc.IsNamed())
	tt.Equal(t, slip.Symbol("vector"), sc.RepType())
	tt.Equal(t, 0, sc.InitialOffset())
	tt.Equal(t, (*cl.StructureClass)(nil), sc.Include())
	tt.Equal(t, nil, sc.PrintFunction())
	tt.Equal(t, nil, sc.PrintObject())
}

func TestStructureClassConstructors(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`
(defstruct (ctortest
  (:constructor make-ct)
  (:constructor new-ct (x))) x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("ctortest")
	ctors := sc.Constructors()
	tt.Equal(t, 2, len(ctors))
}

func TestStructureClassIncludeGetter(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`
(defstruct incbase x)
(defstruct (incchild (:include incbase)) y)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("incchild")
	parent := sc.Include()
	tt.NotNil(t, parent)
	tt.Equal(t, "incbase", parent.Name())
}

func TestMustFindStructureClass(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct mustfind x)`, scope).Eval(scope, nil)
	sc := cl.MustFindStructureClass(scope, 0, "mustfind")
	tt.NotNil(t, sc)
	tt.Equal(t, "mustfind", sc.Name())
}

func TestMustFindStructureClassPanic(t *testing.T) {
	scope := slip.NewScope()
	defer func() {
		r := recover()
		tt.NotNil(t, r)
	}()
	cl.MustFindStructureClass(scope, 0, "nonexistent-struct")
}

// ============================================================================
// StructureObject tests
// ============================================================================

func TestStructureObjectString(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct sostr x y)
(make-sostr :x 1 :y 2)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	str := obj.String()
	tt.Equal(t, true, contains(str, "#S("))
	tt.Equal(t, true, contains(str, "sostr"))
}

func TestStructureObjectSimplify(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct sosimp a)
(make-sosimp :a 42)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	simp := obj.Simplify()
	m, ok := simp.(map[string]any)
	tt.Equal(t, true, ok)
	tt.Equal(t, "sosimp", m["type"])
}

func TestStructureObjectEqual(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct soeq x)
(list (make-soeq :x 1) (make-soeq :x 1) (make-soeq :x 2))`, scope)
	result := code.Eval(scope, nil)
	list := result.(slip.List)
	obj1 := list[0].(*cl.StructureObject)
	obj2 := list[1].(*cl.StructureObject)
	obj3 := list[2].(*cl.StructureObject)
	tt.Equal(t, true, obj1.Equal(obj2))
	tt.Equal(t, false, obj1.Equal(obj3))
	tt.Equal(t, false, obj1.Equal(slip.Fixnum(1)))
}

func TestStructureObjectHierarchy(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct sohier x)
(make-sohier :x 1)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	hier := obj.Hierarchy()
	tt.Equal(t, true, len(hier) >= 3)
	tt.Equal(t, slip.Symbol("sohier"), hier[0])
}

func TestStructureObjectEval(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct soeval x)
(make-soeval :x 1)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	evaled := obj.Eval(scope, 0)
	tt.Equal(t, obj, evaled)
}

func TestStructureObjectClass(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct soclass x)
(make-soclass :x 1)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	class := obj.Class()
	tt.Equal(t, "soclass", class.Name())
}

func TestStructureObjectIsA(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct isabase x)
(defstruct (isachild (:include isabase)) y)
(make-isachild :x 1 :y 2)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	tt.Equal(t, true, obj.IsA("isachild"))
	tt.Equal(t, true, obj.IsA("isabase"))
	tt.Equal(t, false, obj.IsA("other"))
}

func TestStructureObjectInit(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct soinit x y)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("soinit")
	obj := cl.NewStructureObject(sc)
	obj.Init(scope, slip.List{slip.Symbol(":x"), slip.Fixnum(10), slip.Symbol(":y"), slip.Fixnum(20)}, 0)
	x, _ := obj.SlotValue(slip.Symbol("x"))
	y, _ := obj.SlotValue(slip.Symbol("y"))
	tt.Equal(t, slip.Fixnum(10), x)
	tt.Equal(t, slip.Fixnum(20), y)
}

func TestStructureObjectSynchronized(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct sosync x)
(make-sosync :x 1)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	tt.Equal(t, false, obj.Synchronized())
	obj.SetSynchronized(true)
	tt.Equal(t, true, obj.Synchronized())
	obj.SetSynchronized(false)
	tt.Equal(t, false, obj.Synchronized())
}

func TestStructureObjectSlotNames(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct sonames a b c)
(make-sonames :a 1 :b 2 :c 3)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	names := obj.SlotNames()
	tt.Equal(t, 3, len(names))
}

func TestStructureObjectSetSlotValue(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct soset x)
(make-soset :x 1)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	ok := obj.SetSlotValue(slip.Symbol("x"), slip.Fixnum(99))
	tt.Equal(t, true, ok)
	val, _ := obj.SlotValue(slip.Symbol("x"))
	tt.Equal(t, slip.Fixnum(99), val)

	// Test nonexistent slot
	ok2 := obj.SetSlotValue(slip.Symbol("nonexistent"), slip.Fixnum(1))
	tt.Equal(t, false, ok2)
}

func TestStructureObjectSetSlotValueReadOnly(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct soro (x 0 :read-only t))
(make-soro :x 42)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	ok := obj.SetSlotValue(slip.Symbol("x"), slip.Fixnum(99))
	tt.Equal(t, false, ok) // Should fail for read-only
}

func TestStructureObjectGetSlotByIndex(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct soindex a b)
(make-soindex :a 10 :b 20)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	tt.Equal(t, slip.Fixnum(10), obj.GetSlotByIndex(0))
	tt.Equal(t, slip.Fixnum(20), obj.GetSlotByIndex(1))
	tt.Equal(t, nil, obj.GetSlotByIndex(-1))
	tt.Equal(t, nil, obj.GetSlotByIndex(100))
}

func TestStructureObjectGetMethod(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct someth x)
(make-someth :x 1)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	tt.Equal(t, (*slip.Method)(nil), obj.GetMethod("anything"))
}

func TestStructureObjectMethodNames(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct somnames x)
(make-somnames :x 1)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	tt.Equal(t, slip.List(nil), obj.MethodNames())
}

func TestStructureObjectID(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct soid x)
(list (make-soid :x 1) (make-soid :x 2))`, scope)
	result := code.Eval(scope, nil)
	list := result.(slip.List)
	obj1 := list[0].(*cl.StructureObject)
	obj2 := list[1].(*cl.StructureObject)
	tt.Equal(t, true, obj1.ID() != obj2.ID())
	tt.Equal(t, true, obj1.ID() > 0)
}

func TestStructureObjectReceive(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct sorcv x)
(make-sorcv :x 1)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	defer func() {
		r := recover()
		tt.NotNil(t, r) // Should panic
	}()
	obj.Receive(scope, "test", nil, 0)
}

func TestStructureObjectDup(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct sodup x)
(make-sodup :x 42)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	dup := obj.Dup()
	dupObj := dup.(*cl.StructureObject)
	tt.Equal(t, true, obj.ID() != dupObj.ID())
	x1, _ := obj.SlotValue(slip.Symbol("x"))
	x2, _ := dupObj.SlotValue(slip.Symbol("x"))
	tt.Equal(t, x1, x2)
}

// ============================================================================
// StructureSlot tests
// ============================================================================

func TestStructureSlotGetters(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct ssget (myslot 42 :type fixnum :read-only t))`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("ssget")
	slot := sc.GetSlot("myslot")
	tt.NotNil(t, slot)
	tt.Equal(t, "myslot", slot.Name())
	tt.Equal(t, 0, slot.Index())
	tt.Equal(t, true, slot.IsReadOnly())
	tt.Equal(t, slip.Fixnum(42), slot.Initform())
	tt.Equal(t, slip.Symbol("fixnum"), slot.SlotType())
}

// ============================================================================
// Additional parseStructureOptions coverage
// ============================================================================

func TestDefstructInitialOffset(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (offtest (:type vector) (:initial-offset 2)) x y)
(let ((v (make-offtest :x 1 :y 2)))
  (list (svref v 2) (svref v 3)))`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestDefstructPrintFunction(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (pftest (:print-function (lambda (obj stream depth) (write "custom" :stream stream)))) x)`,
		Validate: func(t *testing.T, result slip.Object) {
			tt.Equal(t, slip.Symbol("pftest"), result)
			sc := cl.FindStructureClass("pftest")
			tt.NotNil(t, sc.PrintFunction())
		},
	}).Test(t)
}

func TestDefstructPrintObject(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (potest (:print-object (lambda (obj stream) (write "custom" :stream stream)))) x)`,
		Validate: func(t *testing.T, result slip.Object) {
			tt.Equal(t, slip.Symbol("potest"), result)
			sc := cl.FindStructureClass("potest")
			tt.NotNil(t, sc.PrintObject())
		},
	}).Test(t)
}

func TestDefstructTypedVectorReadOnly(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vro (:type vector) (:named)) (x 0 :read-only t))
(let ((v (make-vro :x 42)))
  (setf (vro-x v) 99))`,
		Panics: true,
	}).Test(t)
}

func TestDefstructTypedListReadOnly(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (lro (:type list) (:named)) (x 0 :read-only t))
(let ((l (make-lro :x 42)))
  (setf (lro-x l) 99))`,
		Panics: true,
	}).Test(t)
}

// ============================================================================
// More BOA pattern coverage
// ============================================================================

func TestDefstructTypedBOAKey(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vboakey (:type vector) (:named) (:constructor make-vboakey (&key x y))) x y)
(let ((v (make-vboakey :y 2 :x 1)))
  (list (vboakey-x v) (vboakey-y v)))`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestDefstructTypedBOAAux(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vboaaux (:type vector) (:named) (:constructor make-vboaaux (x &aux (y 50)))) x y)
(let ((v (make-vboaaux 10)))
  (list (vboaaux-x v) (vboaaux-y v)))`,
		Expect: "(10 50)",
	}).Test(t)
}

func TestDefstructTypedBOARest(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vboarest (:type vector) (:named) (:constructor make-vboarest (x &rest y))) x y)
(let ((v (make-vboarest 1 2 3)))
  (list (vboarest-x v) (vboarest-y v)))`,
		Expect: "(1 (2 3))",
	}).Test(t)
}

func TestDefstructBOASimpleOptional(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (simopt (:constructor make-simopt (x &optional y))) x y)
(list (simopt-y (make-simopt 1)) (simopt-y (make-simopt 1 2)))`,
		Expect: "(nil 2)",
	}).Test(t)
}

func TestDefstructTypedBOASimpleOptional(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vsimopt (:type vector) (:named) (:constructor make-vsimopt (x &optional y))) x y)
(list (vsimopt-y (make-vsimopt 1)) (vsimopt-y (make-vsimopt 1 2)))`,
		Expect: "(nil 2)",
	}).Test(t)
}

func TestDefstructBOAAllowOtherKeys(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (aokey (:constructor make-aokey (&key x &allow-other-keys))) x)
(aokey-x (make-aokey :x 42 :extra 99))`,
		Expect: "42",
	}).Test(t)
}

// ============================================================================
// Error path tests
// ============================================================================

func TestDefstructSlotNotFound(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct snftest x)
(make-snftest :x 1)`, scope)
	result := code.Eval(scope, nil)
	obj := result.(*cl.StructureObject)
	_, found := obj.SlotValue(slip.Symbol("nonexistent"))
	tt.Equal(t, false, found)
}

func TestDefstructFindNonexistent(t *testing.T) {
	sc := cl.FindStructureClass("totally-nonexistent-structure")
	tt.Equal(t, (*cl.StructureClass)(nil), sc)
}

// ============================================================================
// Additional coverage tests
// ============================================================================

func TestStructureClassLoadFormWithOptions(t *testing.T) {
	scope := slip.NewScope()
	// Test LoadForm with non-default options
	slip.ReadString(`
(defstruct (lfopt
  (:conc-name lfo-)
  (:copier nil)
  (:predicate nil)
  (:type vector)) x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("lfopt")
	form := sc.LoadForm()
	tt.NotNil(t, form)
}

func TestStructureClassLoadFormWithInclude(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`
(defstruct lfbase x)
(defstruct (lfinc (:include lfbase)) y)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("lfinc")
	form := sc.LoadForm()
	tt.NotNil(t, form)
}

func TestStructureClassLoadFormSlotWithType(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct lfslot (x nil :type list))`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("lfslot")
	form := sc.LoadForm()
	tt.NotNil(t, form)
}

func TestStructureClassDescribeWithInclude(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`
(defstruct descbase x)
(defstruct (descinc (:include descbase)) y)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("descinc")
	b := sc.Describe(nil, 0, 80, false)
	str := string(b)
	tt.Equal(t, true, contains(str, "Includes"))
}

func TestStructureClassDescribeWithType(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (desctype (:type vector) (:named)) x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("desctype")
	b := sc.Describe(nil, 0, 80, false)
	str := string(b)
	tt.Equal(t, true, contains(str, "Type"))
}

func TestStructureClassDescribeNoCopier(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (descnc (:copier nil)) x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("descnc")
	b := sc.Describe(nil, 0, 80, false)
	str := string(b)
	tt.Equal(t, true, len(str) > 0)
}

func TestStructureClassDescribeNoPredicate(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`(defstruct (descnp (:predicate nil)) x)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("descnp")
	b := sc.Describe(nil, 0, 80, false)
	str := string(b)
	tt.Equal(t, true, len(str) > 0)
}

func TestStructureClassInheritsDeep(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadString(`
(defstruct inhroot x)
(defstruct (inhmid (:include inhroot)) y)
(defstruct (inhleaf (:include inhmid)) z)`, scope).Eval(scope, nil)
	sc := cl.FindStructureClass("inhleaf")
	root := cl.FindStructureClass("inhroot")
	tt.Equal(t, true, sc.Inherits(root))
}

func TestStructureObjectEqualDifferentTypes(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct eqtype1 x)
(defstruct eqtype2 x)
(list (make-eqtype1 :x 1) (make-eqtype2 :x 1))`, scope)
	result := code.Eval(scope, nil)
	list := result.(slip.List)
	obj1 := list[0].(*cl.StructureObject)
	obj2 := list[1].(*cl.StructureObject)
	tt.Equal(t, false, obj1.Equal(obj2)) // Different types
}

func TestDefstructTypedVectorNotNamed(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vnoname (:type vector)) x y)
(let ((v (make-vnoname :x 1 :y 2)))
  (list (vnoname-x v) (vnoname-y v)))`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestDefstructTypedListNotNamed(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (lnoname (:type list)) x y)
(let ((l (make-lnoname :x 1 :y 2)))
  (list (lnoname-x l) (lnoname-y l)))`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestDefstructTypedVectorInitOffset(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vioff (:type vector) (:initial-offset 1) (:named)) x)
(let ((v (make-vioff :x 42)))
  (vioff-x v))`,
		Expect: "42",
	}).Test(t)
}

func TestDefstructTypedListInitOffset(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (lioff (:type list) (:initial-offset 1) (:named)) x)
(let ((l (make-lioff :x 42)))
  (lioff-x l))`,
		Expect: "42",
	}).Test(t)
}

func TestDefstructTypedBOAKeyDefault(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vboakd (:type vector) (:named) (:constructor make-vboakd (&key (x 10) (y 20)))) x y)
(let ((v (make-vboakd)))
  (list (vboakd-x v) (vboakd-y v)))`,
		Expect: "(10 20)",
	}).Test(t)
}

func TestDefstructBOANonSlotParam(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (boanon (:constructor make-boanon (x extra))) x)
(boanon-x (make-boanon 42 99))`,
		Expect: "42",
	}).Test(t)
}

func TestDefstructTypedBOANonSlotParam(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vboanon (:type vector) (:named) (:constructor make-vboanon (x extra))) x)
(vboanon-x (make-vboanon 42 99))`,
		Expect: "42",
	}).Test(t)
}

func TestDefstructTypedListBOAKey(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (lboakey (:type list) (:named) (:constructor make-lboakey (&key x y))) x y)
(let ((l (make-lboakey :x 5 :y 6)))
  (list (lboakey-x l) (lboakey-y l)))`,
		Expect: "(5 6)",
	}).Test(t)
}

func TestDefstructTypedListBOAOptional(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (lboaopt (:type list) (:named) (:constructor make-lboaopt (x &optional (y 77)))) x y)
(list (lboaopt-y (make-lboaopt 1)) (lboaopt-y (make-lboaopt 1 2)))`,
		Expect: "(77 2)",
	}).Test(t)
}

func TestDefstructSlotSimpleName(t *testing.T) {
	(&sliptest.Macro{
		Source: `(defstruct simpslot x)`,
		Validate: func(t *testing.T, _ slip.Object) {
			sc := cl.FindStructureClass("simpslot")
			slot := sc.GetSlot("x")
			tt.NotNil(t, slot)
			tt.Equal(t, nil, slot.Initform())
			tt.Equal(t, nil, slot.SlotType())
			tt.Equal(t, false, slot.IsReadOnly())
		},
	}).Test(t)
}

func TestDefstructAccessorWrongType(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct accwrong x)
(accwrong-x 42)`,
		Panics: true,
	}).Test(t)
}

func TestDefstructCopierWrongType(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct copwrong x)
(copy-copwrong 42)`,
		Panics: true,
	}).Test(t)
}

func TestDefstructPredicateNonStructure(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct prednon x)
(prednon-p "not a structure")`,
		Expect: "nil",
	}).Test(t)
}

func TestDefstructTypedVectorAccessorWrongType(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vaccwrong (:type vector) (:named)) x)
(vaccwrong-x "not a vector")`,
		Panics: true,
	}).Test(t)
}

func TestDefstructTypedListAccessorWrongType(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (laccwrong (:type list) (:named)) x)
(laccwrong-x "not a list")`,
		Panics: true,
	}).Test(t)
}

func TestDefstructSetfWrongType(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct setfwrong x)
(setf (setfwrong-x 42) 99)`,
		Panics: true,
	}).Test(t)
}

func TestDefstructTypedVectorCopierWrongType(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vcopywrong (:type vector) (:named)) x)
(copy-vcopywrong "not a vector")`,
		Panics: true,
	}).Test(t)
}

func TestDefstructTypedListCopierWrongType(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (lcopywrong (:type list) (:named)) x)
(copy-lcopywrong "not a list")`,
		Panics: true,
	}).Test(t)
}

func TestDefstructTypedVectorSetfWrongType(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vsetfwrong (:type vector) (:named)) x)
(setf (vsetfwrong-x "not a vector") 99)`,
		Panics: true,
	}).Test(t)
}

func TestDefstructTypedListSetfWrongType(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (lsetfwrong (:type list) (:named)) x)
(setf (lsetfwrong-x "not a list") 99)`,
		Panics: true,
	}).Test(t)
}

func TestDefstructLoadFormWithDocs(t *testing.T) {
	(&sliptest.Macro{
		Source: `(defstruct lfdocs "documentation" x)`,
		Validate: func(t *testing.T, _ slip.Object) {
			sc := cl.FindStructureClass("lfdocs")
			form := sc.LoadForm()
			tt.NotNil(t, form)
		},
	}).Test(t)
}

func TestDefstructLoadFormEmptyConcName(t *testing.T) {
	(&sliptest.Macro{
		Source: `(defstruct (lfemptyconc (:conc-name nil)) lfemptyconc-val)`,
		Validate: func(t *testing.T, _ slip.Object) {
			sc := cl.FindStructureClass("lfemptyconc")
			form := sc.LoadForm()
			tt.NotNil(t, form)
		},
	}).Test(t)
}

func TestDefstructLoadFormCustomConstructor(t *testing.T) {
	(&sliptest.Macro{
		Source: `(defstruct (lfcustctor (:constructor new-lfcustctor)) x)`,
		Validate: func(t *testing.T, _ slip.Object) {
			sc := cl.FindStructureClass("lfcustctor")
			form := sc.LoadForm()
			tt.NotNil(t, form)
		},
	}).Test(t)
}

// ============================================================================
// Error path tests for parseStructureOptions
// ============================================================================

func TestDefstructErrorEmptyNameAndOptions(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct ())`, Panics: true}).Test(t)
}

func TestDefstructErrorNameNotSymbol(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (42))`, Panics: true}).Test(t)
}

func TestDefstructErrorNameAndOptionsWrongType(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct 42)`, Panics: true}).Test(t)
}

func TestDefstructErrorUnknownKeywordOption(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt :unknown-option))`, Panics: true}).Test(t)
}

func TestDefstructErrorEmptyListOption(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt ()))`, Panics: true}).Test(t)
}

func TestDefstructErrorOptionKeywordNotSymbol(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (42 foo)))`, Panics: true}).Test(t)
}

func TestDefstructErrorConcNameWrongType(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:conc-name 42)))`, Panics: true}).Test(t)
}

func TestDefstructErrorBOAArglistNotList(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:constructor make-err not-a-list)))`, Panics: true}).Test(t)
}

func TestDefstructErrorConstructorNameNotSymbol(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:constructor 42)))`, Panics: true}).Test(t)
}

func TestDefstructErrorCopierWrongType(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:copier 42)))`, Panics: true}).Test(t)
}

func TestDefstructErrorPredicateWrongType(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:predicate 42)))`, Panics: true}).Test(t)
}

func TestDefstructErrorIncludeMissingName(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:include)))`, Panics: true}).Test(t)
}

func TestDefstructErrorIncludeNameNotSymbol(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:include 42)))`, Panics: true}).Test(t)
}

func TestDefstructErrorIncludeNotDefined(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:include nonexistent-parent)))`, Panics: true}).Test(t)
}

func TestDefstructErrorIncludeTypedInUntyped(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (typedparent (:type vector)) x)
(defstruct (erropt (:include typedparent)) y)`,
		Panics: true,
	}).Test(t)
}

func TestDefstructErrorIncludeDifferentType(t *testing.T) {
	(&sliptest.Macro{
		Source: `
(defstruct (vecparent (:type vector)) x)
(defstruct (erropt (:type list) (:include vecparent)) y)`,
		Panics: true,
	}).Test(t)
}

func TestDefstructErrorInitialOffsetMissingValue(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:type vector) (:initial-offset)))`, Panics: true}).Test(t)
}

func TestDefstructErrorInitialOffsetNotFixnum(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:type vector) (:initial-offset "two")))`, Panics: true}).Test(t)
}

func TestDefstructErrorTypeMissingValue(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:type)))`, Panics: true}).Test(t)
}

func TestDefstructErrorTypeInvalidSymbol(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:type invalid)))`, Panics: true}).Test(t)
}

func TestDefstructErrorTypeListNotVector(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:type (invalid t))))`, Panics: true}).Test(t)
}

func TestDefstructErrorTypeWrongType(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:type 42)))`, Panics: true}).Test(t)
}

func TestDefstructErrorUnknownListOption(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:unknown-option foo)))`, Panics: true}).Test(t)
}

func TestDefstructErrorOptionNotSymbolOrList(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt 42))`, Panics: true}).Test(t)
}

func TestDefstructErrorPrintFunctionWithType(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct (erropt (:type vector) (:print-function (lambda (o s d) nil))))`, Panics: true}).Test(t)
}

// ============================================================================
// Error path tests for NewStructureSlot
// ============================================================================

func TestDefstructErrorSlotEmptyList(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct errslot ())`, Panics: true}).Test(t)
}

func TestDefstructErrorSlotNameNotSymbol(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct errslot (42))`, Panics: true}).Test(t)
}

func TestDefstructErrorSlotOptionMissingValue(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct errslot (x nil :type))`, Panics: true}).Test(t)
}

func TestDefstructErrorSlotTypeTwice(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct errslot (x nil :type fixnum :type string))`, Panics: true}).Test(t)
}

func TestDefstructErrorSlotUnknownOption(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct errslot (x nil :unknown t))`, Panics: true}).Test(t)
}

func TestDefstructErrorSlotNotSymbolOrList(t *testing.T) {
	(&sliptest.Macro{Source: `(defstruct errslot 42)`, Panics: true}).Test(t)
}

// Helper function
func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(s) > 0 && containsHelper(s, substr))
}

func containsHelper(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
