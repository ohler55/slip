// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

func defineBerry(t *testing.T) {
	_ = slip.ReadString(`
(defflavor berry (color) ()
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables)
(defmethod (berry :rot) () (setq color 'brown))
(defmethod (berry :before :color) () (princ "berry before color") (terpri))
(defmethod (berry :after :rot) () (princ "berry after rot") (terpri))
`).Eval(slip.NewScope())
}

func defineBlueberry(t *testing.T) {
	_ = slip.ReadString(`
(defflavor blueberry () (berry))
(defmethod (blueberry :before :rot) () (princ "blueberry before rot") (terpri))
(defmethod (blueberry :after :rot) () (princ "blueberry after rot") (terpri))
`).Eval(slip.NewScope())
}

func TestDefMethodBasic(t *testing.T) {
	defer undefFlavor("berry")
	defineBerry(t)

	f := slip.ReadString("berry").Eval(slip.NewScope())
	sf := f.Simplify()
	require.True(t, true, jp.MustParseString("methods[*][?(@.name == ':rot')].primary").First(sf))
	require.True(t, true, jp.MustParseString("methods[*][?(@.name == ':rot')].after").First(sf))
	require.True(t, true, jp.MustParseString("methods[*][?(@.name == ':color')].before").First(sf))
}

func TestDefMethodInherit(t *testing.T) {
	defer undefFlavors("berry", "blueberry")
	defineBerry(t)
	defineBlueberry(t)

	f := slip.ReadString("blueberry").Eval(slip.NewScope())
	sf := f.Simplify()
	rot := jp.MustParseString("methods[*][?(@.name == ':rot')]").Get(sf)
	require.Equal(t, 2, len(rot))
	require.Equal(t, "blueberry", jp.C("from").First(rot[0]))
	require.Equal(t, true, jp.C("before").First(rot[0]))
	require.Equal(t, true, jp.C("after").First(rot[0]))
	require.Equal(t, "berry", jp.C("from").First(rot[1]))
}

func TestDefMethodArgCount(t *testing.T) {
	defer undefFlavor("berry")
	require.Panics(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry :rot))
`).Eval(slip.NewScope())
	})
}

func TestDefMethodNotList(t *testing.T) {
	defer undefFlavor("berry")
	require.Panics(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod t ())
`).Eval(slip.NewScope())
	})
}

func TestDefMethodLowDesignatorCount(t *testing.T) {
	defer undefFlavor("berry")
	require.Panics(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry) ())
`).Eval(slip.NewScope())
	})
}

func TestDefMethodHighDesignatorCount(t *testing.T) {
	defer undefFlavor("berry")
	require.Panics(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry :before :rot nil) ())
`).Eval(slip.NewScope())
	})
}

func TestDefMethodBadDesignator(t *testing.T) {
	defer undefFlavor("berry")
	require.Panics(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry rot) ())
`).Eval(slip.NewScope())
	})
}

func TestDefMethodNotFlavor(t *testing.T) {
	require.Panics(t, func() {
		_ = slip.ReadString(`(defmethod (nothing :rot) ())`).Eval(slip.NewScope())
	})
}
