// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWithSlotsDirect(t *testing.T) {
	slip.CurrentPackage.Remove("quux")

	(&sliptest.Function{
		Source: `(defclass quux () ((x :initarg :x)))`,
		Expect: "#<standard-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots (x) q
                     x))`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots (x) q
                     (list x)))`,
		Expect: "(3)",
	}).Test(t)
}

func TestWithSlotsAlias(t *testing.T) {
	slip.CurrentPackage.Remove("quux")

	(&sliptest.Function{
		Source: `(defclass quux () ((x :initarg :x)))`,
		Expect: "#<standard-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots ((z x)) q
                     z))`,
		Expect: "3",
	}).Test(t)
}

func TestWithSlotsSetf(t *testing.T) {
	slip.CurrentPackage.Remove("quux")

	(&sliptest.Function{
		Source: `(defclass quux () ((x :initarg :x)))`,
		Expect: "#<standard-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots ((z x)) q
                     (setf (slot-value q 'x) 4)
                     (list z (slot-value q 'x))))`,
		Expect: "(4 4)",
	}).Test(t)
}

func TestWithSlotsSetq(t *testing.T) {
	slip.CurrentPackage.Remove("quux")

	(&sliptest.Function{
		Source: `(defclass quux () ((x :initarg :x)))`,
		Expect: "#<standard-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots ((z x)) q
                     (setq z 4)
                     (list z (slot-value q 'x))))`,
		Expect: "(4 4)",
	}).Test(t)
}

func TestWithSlotsNotInstance(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-slots ((z x)) 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithSlotsSlotEntryNotList(t *testing.T) {
	slip.CurrentPackage.Remove("quux")

	(&sliptest.Function{
		Source: `(defclass quux () ((x :initarg :x)))`,
		Expect: "#<standard-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots x q nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithSlotsBadSlotEntry(t *testing.T) {
	slip.CurrentPackage.Remove("quux")

	(&sliptest.Function{
		Source: `(defclass quux () ((x :initarg :x)))`,
		Expect: "#<standard-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots (1) q nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots ((1 x)) q nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots ((x 1)) q nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots ((x)) q nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithSlotsMissingSlot(t *testing.T) {
	slip.CurrentPackage.Remove("quux")

	(&sliptest.Function{
		Source: `(defclass quux () ((x :initarg :x)))`,
		Expect: "#<standard-class quux>",
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots (z) q nil))`,
		PanicType: slip.CellErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((q (make-instance 'quux :x 3)))
                   (with-slots ((x z)) q nil))`,
		PanicType: slip.CellErrorSymbol,
	}).Test(t)
}
