// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

func TestDefstructBasic(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`(defstruct point x y)`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Symbol("point"), result)

	// Check that the class was registered
	class := slip.FindClass("point")
	tt.NotNil(t, class)
	tt.Equal(t, "point", class.Name())
}

func TestDefstructConstructor(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct point x y)
(make-point :x 10 :y 20)`, scope)
	result := code.Eval(scope, nil)

	obj, ok := result.(*cl.StructureObject)
	tt.Equal(t, true, ok)
	tt.Equal(t, "point", obj.Type.Name())

	x, _ := obj.SlotValue(slip.Symbol("x"))
	y, _ := obj.SlotValue(slip.Symbol("y"))
	tt.Equal(t, slip.Fixnum(10), x)
	tt.Equal(t, slip.Fixnum(20), y)
}

func TestDefstructPredicate(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct point x y)
(point-p (make-point :x 1 :y 2))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.TrueSymbol, result)

	// Test with non-point
	code2 := slip.ReadString(`(point-p 42)`, scope)
	result2 := code2.Eval(scope, nil)
	tt.Equal(t, nil, result2)
}

func TestDefstructAccessors(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct point x y)
(let ((p (make-point :x 5 :y 10)))
  (list (point-x p) (point-y p)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(5), slip.Fixnum(10)}, result)
}

func TestDefstructSetf(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct point x y)
(let ((p (make-point :x 1 :y 2)))
  (setf (point-x p) 100)
  (point-x p))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(100), result)
}

func TestDefstructCopier(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct point x y)
(let* ((p1 (make-point :x 1 :y 2))
       (p2 (copy-point p1)))
  (setf (point-x p2) 999)
  (list (point-x p1) (point-x p2)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(1), slip.Fixnum(999)}, result)
}

func TestDefstructInitform(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct counter (value 0))
(counter-value (make-counter))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(0), result)
}

func TestDefstructConcName(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (person (:conc-name p-)) name age)
(let ((joe (make-person :name "Joe" :age 30)))
  (list (p-name joe) (p-age joe)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.String("Joe"), slip.Fixnum(30)}, result)
}

func TestDefstructConcNameNil(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (thing (:conc-name nil)) value)
(let ((t1 (make-thing :value 42)))
  (value t1))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(42), result)
}

func TestDefstructCustomConstructor(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (point (:constructor create-point)) x y)
(let ((p (create-point :x 5 :y 6)))
  (list (point-x p) (point-y p)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(5), slip.Fixnum(6)}, result)
}

func TestDefstructNoCopier(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`(defstruct (nocopier-point (:copier nil)) x y)`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Symbol("nocopier-point"), result)

	// copy-nocopier-point should not exist
	fn := slip.FindFunc("copy-nocopier-point")
	tt.Equal(t, (*slip.FuncInfo)(nil), fn)
}

func TestDefstructNoPredicate(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`(defstruct (blob (:predicate nil)) data)`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Symbol("blob"), result)

	// blob-p should not exist
	fn := slip.FindFunc("blob-p")
	tt.Equal(t, (*slip.FuncInfo)(nil), fn)
}

func TestDefstructInclude(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct point2d x y)
(defstruct (point3d (:include point2d)) z)
(let ((p (make-point3d :x 1 :y 2 :z 3)))
  (list (point3d-x p) (point3d-y p) (point3d-z p)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}, result)
}

func TestDefstructIncludeTypep(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct base-shape)
(defstruct (rectangle (:include base-shape)) width height)
(let ((r (make-rectangle :width 10 :height 20)))
  (list (rectangle-p r) (base-shape-p r)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.TrueSymbol, slip.TrueSymbol}, result)
}

func TestDefstructReadOnly(t *testing.T) {
	scope := slip.NewScope()
	defer func() {
		r := recover()
		tt.NotNil(t, r) // Expect panic on setf of read-only slot
	}()

	code := slip.ReadString(`
(defstruct immutable (id 0 :read-only t))
(let ((obj (make-immutable :id 42)))
  (setf (immutable-id obj) 999))`, scope)
	code.Eval(scope, nil)
}

func TestDefstructDocumentation(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct doc-point
  "A 2D point structure"
  x y)`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Symbol("doc-point"), result)

	class := slip.FindClass("doc-point")
	tt.Equal(t, "A 2D point structure", class.Documentation())
}

func TestDefstructTypeVector(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (vec-point (:type vector) (:named)) x y)
(let ((p (make-vec-point :x 5 :y 10)))
  (list (vec-point-x p) (vec-point-y p) (vectorp p)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(5), slip.Fixnum(10), slip.True}, result)
}

func TestDefstructTypeVectorNamed(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (named-vec (:type vector) (:named)) a b)
(let ((v (make-named-vec :a 1 :b 2)))
  (svref v 0))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Symbol("named-vec"), result)
}

func TestDefstructTypeList(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (list-point (:type list) (:named)) x y)
(let ((p (make-list-point :x 3 :y 4)))
  (list (list-point-x p) (list-point-y p) (listp p)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(3), slip.Fixnum(4), slip.True}, result)
}

func TestDefstructBOAConstructor(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (point (:constructor make-pt (x y)))
  x y)
(let ((p (make-pt 7 8)))
  (list (point-x p) (point-y p)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(7), slip.Fixnum(8)}, result)
}

func TestDefstructBOAOptional(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (point (:constructor make-pt (x &optional (y 0))))
  x y)
(list (point-y (make-pt 5)) (point-y (make-pt 5 10)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(0), slip.Fixnum(10)}, result)
}

func TestDefstructMultipleConstructors(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (point
             (:constructor make-point)
             (:constructor make-pt (x y)))
  x y)
(list
  (point-x (make-point :x 1 :y 2))
  (point-x (make-pt 3 4)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(1), slip.Fixnum(3)}, result)
}

func TestDefstructPrint(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct simple-point x y)
(princ-to-string (make-simple-point :x 1 :y 2))`, scope)
	result := code.Eval(scope, nil)
	// Should print in #S(...) format
	str, ok := result.(slip.String)
	tt.Equal(t, true, ok)
	tt.Equal(t, true, len(str) > 0)
}

// Additional tests for coverage

func TestDefstructTypedVectorPredicate(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (tvec (:type vector) (:named)) x)
(list (tvec-p (make-tvec :x 1)) (tvec-p #(not-tvec 1)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.TrueSymbol, nil}, result)
}

func TestDefstructTypedListPredicate(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (tlist (:type list) (:named)) x)
(list (tlist-p (make-tlist :x 1)) (tlist-p '(not-tlist 1)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.TrueSymbol, nil}, result)
}

func TestDefstructTypedVectorCopier(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (vcopy (:type vector) (:named)) a b)
(let* ((v1 (make-vcopy :a 1 :b 2))
       (v2 (copy-vcopy v1)))
  (setf (vcopy-a v2) 99)
  (list (vcopy-a v1) (vcopy-a v2)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(1), slip.Fixnum(99)}, result)
}

func TestDefstructTypedListCopier(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (lcopy (:type list) (:named)) a b)
(let* ((l1 (make-lcopy :a 1 :b 2))
       (l2 (copy-lcopy l1)))
  (list (lcopy-a l1) (lcopy-a l2)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(1), slip.Fixnum(1)}, result)
}

func TestDefstructTypedVectorSetf(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (vsetf (:type vector) (:named)) x)
(let ((v (make-vsetf :x 10)))
  (setf (vsetf-x v) 20)
  (vsetf-x v))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(20), result)
}

func TestDefstructTypedBOAConstructor(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (vboa (:type vector) (:named) (:constructor make-vboa (x y))) x y)
(let ((v (make-vboa 5 6)))
  (list (vboa-x v) (vboa-y v)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(5), slip.Fixnum(6)}, result)
}

func TestDefstructTypedBOAOptional(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (vboa2 (:type vector) (:named) (:constructor make-vboa2 (x &optional (y 99)))) x y)
(list (vboa2-y (make-vboa2 1)) (vboa2-y (make-vboa2 1 2)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(99), slip.Fixnum(2)}, result)
}

func TestDefstructTypedListBOA(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (lboa (:type list) (:named) (:constructor make-lboa (a b))) a b)
(let ((l (make-lboa 7 8)))
  (list (lboa-a l) (lboa-b l)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(7), slip.Fixnum(8)}, result)
}

func TestDefstructBOAKey(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (kpoint (:constructor make-kpt (&key x y))) x y)
(let ((p (make-kpt :y 20 :x 10)))
  (list (kpoint-x p) (kpoint-y p)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(10), slip.Fixnum(20)}, result)
}

func TestDefstructBOAKeyDefault(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (kdpoint (:constructor make-kdpt (&key (x 1) (y 2)))) x y)
(list (kpoint-x (make-kdpt)) (kpoint-y (make-kdpt :y 99)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(1), slip.Fixnum(99)}, result)
}

func TestDefstructBOAAux(t *testing.T) {
	scope := slip.NewScope()
	// &aux with constant initform (referencing other params not yet supported)
	code := slip.ReadString(`
(defstruct (auxpt (:constructor make-auxpt (x &aux (y 99)))) x y)
(let ((p (make-auxpt 5)))
  (list (auxpt-x p) (auxpt-y p)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(5), slip.Fixnum(99)}, result)
}

func TestDefstructBOARest(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (restpt (:constructor make-restpt (x &rest y))) x y)
(let ((p (make-restpt 1 2 3 4)))
  (list (restpt-x p) (restpt-y p)))`, scope)
	result := code.Eval(scope, nil)
	expected := slip.List{slip.Fixnum(1), slip.List{slip.Fixnum(2), slip.Fixnum(3), slip.Fixnum(4)}}
	tt.Equal(t, expected, result)
}

func TestDefstructCustomPredicate(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (cpred (:predicate is-cpred)) val)
(is-cpred (make-cpred :val 1))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.TrueSymbol, result)
}

func TestDefstructCustomCopier(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (ccopy (:copier dup-ccopy)) val)
(ccopy-val (dup-ccopy (make-ccopy :val 42)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(42), result)
}

func TestDefstructNoConstructor(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`(defstruct (noctor (:constructor nil)) val)`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Symbol("noctor"), result)

	// make-noctor should not exist
	fn := slip.FindFunc("make-noctor")
	tt.Equal(t, (*slip.FuncInfo)(nil), fn)
}

func TestDefstructSlotType(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct typed-slot (count 0 :type fixnum))
(typed-slot-count (make-typed-slot :count 10))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(10), result)
}

func TestDefstructTypeVectorElement(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defstruct (vecelem (:type (vector t))) a b)
(let ((v (make-vecelem :a 1 :b 2)))
  (list (vecelem-a v) (vecelem-b v)))`, scope)
	result := code.Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Fixnum(1), slip.Fixnum(2)}, result)
}
