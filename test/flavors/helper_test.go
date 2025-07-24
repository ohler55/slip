// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"fmt"
	"testing"

	"github.com/ohler55/slip"
)

func undefFlavors(fns ...string) {
	for _, fn := range fns {
		undefFlavor(fn)
	}
}

func undefFlavor(fn string) {
	defer func() { _ = recover() }()
	scope := slip.NewScope()
	slip.ReadString(fmt.Sprintf("(undefflavor '%s)", fn), scope).Eval(scope, nil)
}

func defineBerry(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor berry (color) ()
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables)
(defmethod (berry :rot) () "When berries rot they turn brown." (setq color 'brown))
(defmethod (berry :before :color) () (princ "berry before color") (terpri))
(defmethod (berry :after :rot) () (princ "berry after rot") (terpri))
`, scope).Eval(scope, nil)
}

func defineBlueberry(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor blueberry () (berry))
(defmethod (blueberry :before :rot) () "When blueberries rot they turn mushy." (princ "blueberry before rot") (terpri))
(defmethod (blueberry :after :rot) () (princ "blueberry after rot") (terpri))
`, scope).Eval(scope, nil)
}
