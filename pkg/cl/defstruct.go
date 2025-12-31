// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defstruct{Function: slip.Function{Name: "defstruct", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defstruct",
			Args: []*slip.DocArg{
				{
					Name: "name-and-options",
					Type: "symbol or list",
					Text: "Either a symbol naming the structure, or a list of the name followed by structure options.",
				},
				{Name: slip.AmpOptional},
				{
					Name: "documentation",
					Type: "string",
					Text: "Documentation string for the structure.",
				},
				{Name: slip.AmpRest},
				{
					Name: "slot-descriptions",
					Type: "list",
					Text: `Each slot-description is either a symbol (the slot name) or a list of
(slot-name default-value slot-option*). Slot options are :type and :read-only.`,
				},
			},
			Return: "symbol",
			Text: `__defstruct__ defines a new structure type. It automatically creates:
- A predicate function _name-p_
- A constructor function _make-name_
- A copier function _copy-name_
- Reader functions for each slot

Structure options include:
  __:conc-name__ - prefix for accessor names (default: name-)
  __:constructor__ - custom constructor name or BOA constructor spec
  __:copier__ - copier function name (nil to suppress)
  __:include__ - inherit slots from another structure
  __:initial-offset__ - skip N slots at start
  __:named__ - include structure name in representation
  __:predicate__ - predicate function name (nil to suppress)
  __:print-function__ - custom print function
  __:print-object__ - custom print-object method
  __:type__ - representation type (vector or list)
`,
			Examples: []string{
				"(defstruct point x y) => point",
				"(defstruct (person (:conc-name p-)) name age) => person",
				"(defstruct (3d-point (:include point)) z) => 3d-point",
			},
		}, &slip.CLPkg)
}

// Defstruct represents the defstruct macro.
type Defstruct struct {
	slip.Function
}

// Call evaluates the defstruct form.
func (f *Defstruct) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	d2 := depth + 1

	// Parse name-and-options
	var name string
	var options slip.List

	switch na := args[0].(type) {
	case slip.Symbol:
		name = string(na)
	case slip.List:
		if len(na) == 0 {
			slip.ErrorPanic(s, depth, "name-and-options cannot be empty")
		}
		if sym, ok := na[0].(slip.Symbol); ok {
			name = string(sym)
		} else {
			slip.TypePanic(s, depth, "structure name", na[0], "symbol")
		}
		options = na[1:]
	default:
		slip.TypePanic(s, depth, "name-and-options", args[0], "symbol", "list")
	}

	// Create the structure class
	sc := NewStructureClass(name, slip.CurrentPackage)

	// Parse options
	parseStructureOptions(s, sc, options, d2)

	// Parse remaining arguments (documentation and slots)
	argIndex := 1

	// Check for documentation string
	if argIndex < len(args) {
		if doc, ok := args[argIndex].(slip.String); ok {
			sc.SetDocumentation(string(doc))
			argIndex++
		}
	}

	// Handle :include - copy parent slots first
	if sc.include != nil {
		slotIndex := sc.initialOffset
		for _, parentSlot := range sc.include.slots {
			newSlot := parentSlot.Copy(slotIndex)
			sc.AddSlot(newSlot)
			slotIndex++
		}
	} else {
		// Apply initial offset by adding placeholder slots
		// (In practice, initial-offset is mainly for :type structures)
	}

	// Parse slot descriptions
	startIndex := len(sc.slots)
	for i := argIndex; i < len(args); i++ {
		slotIndex := startIndex + (i - argIndex)
		slot := NewStructureSlot(s, args[i], slotIndex, d2)

		// Check if this overrides an included slot
		if existing := sc.GetSlot(slot.name); existing != nil {
			// Override: update the existing slot's properties
			if slot.initform != nil {
				existing.initform = slot.initform
			}
			if slot.slotType != nil {
				existing.slotType = slot.slotType
			}
			if slot.readOnly && !existing.readOnly {
				existing.readOnly = true
			} else if !slot.readOnly && existing.readOnly {
				slip.ErrorPanic(s, d2, "cannot make read-only slot %s writable", slot.name)
			}
		} else {
			slot.index = len(sc.slots)
			sc.AddSlot(slot)
		}
	}

	// Build precedence list for typep
	sc.BuildPrecedence()

	// Register the structure class
	slip.RegisterClass(name, sc)

	// Generate and register functions based on representation type
	if sc.repType == "" {
		// Standard structure representation
		generateStandardFunctions(s, sc, d2)
	} else {
		// :type vector or :type list representation
		generateTypedFunctions(s, sc, d2)
	}

	return slip.Symbol(name)
}

// parseStructureOptions parses the options in a defstruct name-and-options list.
func parseStructureOptions(s *slip.Scope, sc *StructureClass, options slip.List, depth int) {
	constructorSeen := false

	for _, opt := range options {
		switch o := opt.(type) {
		case slip.Symbol:
			// Single keyword option (e.g., :named)
			switch o {
			case slip.Symbol(":named"):
				sc.SetNamed(true)
			default:
				slip.ErrorPanic(s, depth, "unknown structure option: %s", o)
			}

		case slip.List:
			if len(o) == 0 {
				slip.ErrorPanic(s, depth, "structure option cannot be empty list")
			}
			key, ok := o[0].(slip.Symbol)
			if !ok {
				slip.TypePanic(s, depth, "structure option keyword", o[0], "symbol")
			}

			switch key {
			case slip.Symbol(":conc-name"):
				if len(o) == 1 || o[1] == nil {
					sc.SetConcName("")
				} else if sym, ok := o[1].(slip.Symbol); ok {
					sc.SetConcName(string(sym))
				} else {
					slip.TypePanic(s, depth, ":conc-name", o[1], "symbol", "nil")
				}

			case slip.Symbol(":constructor"):
				if !constructorSeen {
					sc.ClearConstructors()
					constructorSeen = true
				}
				if len(o) == 1 {
					// (:constructor) - use default name
					sc.AddConstructor("make-"+sc.name, nil)
				} else if o[1] == nil {
					// (:constructor nil) - no constructor
					// Already cleared, nothing to add
				} else if sym, ok := o[1].(slip.Symbol); ok {
					if len(o) == 2 {
						// (:constructor name) - keyword constructor with custom name
						sc.AddConstructor(string(sym), nil)
					} else if boaList, ok := o[2].(slip.List); ok {
						// (:constructor name arglist) - BOA constructor
						sc.AddConstructor(string(sym), boaList)
					} else {
						slip.TypePanic(s, depth, "BOA constructor arglist", o[2], "list")
					}
				} else {
					slip.TypePanic(s, depth, ":constructor name", o[1], "symbol", "nil")
				}

			case slip.Symbol(":copier"):
				if len(o) == 1 || o[1] == nil {
					sc.SetCopierName("")
				} else if sym, ok := o[1].(slip.Symbol); ok {
					sc.SetCopierName(string(sym))
				} else {
					slip.TypePanic(s, depth, ":copier", o[1], "symbol", "nil")
				}

			case slip.Symbol(":predicate"):
				if len(o) == 1 || o[1] == nil {
					sc.SetPredicateName("")
				} else if sym, ok := o[1].(slip.Symbol); ok {
					sc.SetPredicateName(string(sym))
				} else {
					slip.TypePanic(s, depth, ":predicate", o[1], "symbol", "nil")
				}

			case slip.Symbol(":include"):
				if len(o) < 2 {
					slip.ErrorPanic(s, depth, ":include requires a structure name")
				}
				parentName, ok := o[1].(slip.Symbol)
				if !ok {
					slip.TypePanic(s, depth, ":include structure name", o[1], "symbol")
				}
				parent := FindStructureClass(string(parentName))
				if parent == nil {
					slip.ErrorPanic(s, depth, "included structure %s is not defined", parentName)
				}
				// Check :type compatibility
				if parent.repType != "" && sc.repType == "" {
					slip.ErrorPanic(s, depth, "cannot include typed structure %s in untyped structure", parentName)
				}
				if parent.repType != sc.repType && parent.repType != "" {
					slip.ErrorPanic(s, depth, "included structure %s has different :type", parentName)
				}
				sc.SetInclude(parent)

				// Parse slot modifications in :include
				// (:include parent-name slot-desc*)
				for i := 2; i < len(o); i++ {
					// These are slot modifications - handled when adding slots
				}

			case slip.Symbol(":initial-offset"):
				if len(o) < 2 {
					slip.ErrorPanic(s, depth, ":initial-offset requires a value")
				}
				if n, ok := o[1].(slip.Fixnum); ok {
					sc.SetInitialOffset(int(n))
				} else {
					slip.TypePanic(s, depth, ":initial-offset", o[1], "fixnum")
				}

			case slip.Symbol(":named"):
				sc.SetNamed(true)

			case slip.Symbol(":type"):
				if len(o) < 2 {
					slip.ErrorPanic(s, depth, ":type requires a value")
				}
				switch t := o[1].(type) {
				case slip.Symbol:
					if t == slip.Symbol("vector") || t == slip.Symbol("list") {
						sc.SetRepType(t)
					} else {
						slip.ErrorPanic(s, depth, ":type must be vector or list, got %s", t)
					}
				case slip.List:
					// (vector element-type) form
					if len(t) >= 1 {
						if t[0] == slip.Symbol("vector") {
							sc.SetRepType(slip.Symbol("vector"))
						} else {
							slip.ErrorPanic(s, depth, ":type must be vector or list")
						}
					}
				default:
					slip.TypePanic(s, depth, ":type", o[1], "symbol", "list")
				}

			case slip.Symbol(":print-function"):
				if len(o) >= 2 {
					sc.SetPrintFunction(o[1])
				}

			case slip.Symbol(":print-object"):
				if len(o) >= 2 {
					sc.SetPrintObject(o[1])
				}

			default:
				slip.ErrorPanic(s, depth, "unknown structure option: %s", key)
			}

		default:
			slip.TypePanic(s, depth, "structure option", opt, "symbol", "list")
		}
	}

	// Validate options
	if sc.repType != "" && (sc.printFunc != nil || sc.printObject != nil) {
		slip.ErrorPanic(s, depth, ":print-function/:print-object cannot be used with :type")
	}
	if sc.repType != "" && !sc.named && sc.predicateName != "" {
		// Predicate requires :named for typed structures
		sc.SetPredicateName("")
	}
}

// generateStandardFunctions generates functions for standard structure representation.
func generateStandardFunctions(s *slip.Scope, sc *StructureClass, depth int) {
	// Generate predicate
	if sc.predicateName != "" {
		generatePredicate(sc)
	}

	// Generate constructors
	for _, ctor := range sc.constructors {
		if ctor.boaList != nil {
			generateBOAConstructor(s, sc, ctor.name, ctor.boaList, depth)
		} else {
			generateKeywordConstructor(sc, ctor.name)
		}
	}

	// Generate copier
	if sc.copierName != "" {
		generateCopier(sc)
	}

	// Generate accessors for each slot
	for _, slot := range sc.slots {
		generateAccessor(sc, slot)
	}
}

// generatePredicate creates the type predicate function.
func generatePredicate(sc *StructureClass) {
	name := sc.predicateName
	structName := sc.name

	slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := structPredicate{
				Function:   slip.Function{Name: name, Args: args},
				structName: structName,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: name,
			Args: []*slip.DocArg{
				{Name: "object", Type: "t", Text: "Object to test."},
			},
			Return: "boolean",
			Text:   fmt.Sprintf("Returns t if object is a %s structure.", structName),
		},
	)
}

type structPredicate struct {
	slip.Function
	structName string
}

func (f *structPredicate) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	obj, ok := args[0].(*StructureObject)
	if !ok {
		return nil
	}
	// Check if obj's type matches or inherits from structName
	for _, sym := range obj.Type.precedence {
		if string(sym) == f.structName {
			return slip.TrueSymbol
		}
	}
	return nil
}

// generateKeywordConstructor creates a make-name constructor with keyword args.
func generateKeywordConstructor(sc *StructureClass, name string) {
	structClass := sc // capture for closure

	docArgs := make([]*slip.DocArg, 0, len(sc.slots)*2+1)
	docArgs = append(docArgs, &slip.DocArg{Name: slip.AmpKey})
	for _, slot := range sc.slots {
		docArgs = append(docArgs, &slip.DocArg{
			Name: slot.name,
			Type: "t",
			Text: fmt.Sprintf("Initial value for %s slot.", slot.name),
		})
	}

	slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := structConstructor{
				Function:    slip.Function{Name: name, Args: args},
				structClass: structClass,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   name,
			Args:   docArgs,
			Return: sc.name,
			Text:   fmt.Sprintf("Creates a new %s structure.", sc.name),
		},
	)
}

type structConstructor struct {
	slip.Function
	structClass *StructureClass
}

func (f *structConstructor) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := NewStructureObject(f.structClass)
	d2 := depth + 1

	// First, initialize slots with their initforms
	for _, slot := range f.structClass.slots {
		if slot.initform != nil {
			obj.slots[slot.index] = s.Eval(slot.initform, d2)
		}
	}

	// Then, apply keyword arguments (override initforms)
	for i := 0; i < len(args)-1; i += 2 {
		if sym, ok := args[i].(slip.Symbol); ok {
			name := string(sym)
			if len(name) > 0 && name[0] == ':' {
				name = name[1:]
			}
			if slot := f.structClass.GetSlot(name); slot != nil {
				obj.slots[slot.index] = args[i+1]
			}
		}
	}

	return obj
}

// generateCopier creates the copy-name function.
func generateCopier(sc *StructureClass) {
	name := sc.copierName
	structName := sc.name

	slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := structCopier{
				Function:   slip.Function{Name: name, Args: args},
				structName: structName,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: name,
			Args: []*slip.DocArg{
				{Name: "structure", Type: structName, Text: "Structure to copy."},
			},
			Return: structName,
			Text:   fmt.Sprintf("Creates a shallow copy of a %s structure.", structName),
		},
	)
}

type structCopier struct {
	slip.Function
	structName string
}

func (f *structCopier) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	obj, ok := args[0].(*StructureObject)
	if !ok {
		slip.TypePanic(s, depth, "structure", args[0], f.structName)
	}
	return obj.Dup()
}

// generateAccessor creates the accessor function for a slot.
func generateAccessor(sc *StructureClass, slot *StructureSlot) {
	name := sc.accessorName(slot.name)
	structName := sc.name
	slotIndex := slot.index
	readOnly := slot.readOnly

	slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := structAccessor{
				Function:   slip.Function{Name: name, Args: args},
				structName: structName,
				slotIndex:  slotIndex,
				readOnly:   readOnly,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: name,
			Args: []*slip.DocArg{
				{Name: "structure", Type: structName, Text: "Structure to access."},
			},
			Return: "t",
			Text:   fmt.Sprintf("Returns the %s slot of a %s structure.", slot.name, structName),
		},
	)
}

type structAccessor struct {
	slip.Function
	structName string
	slotIndex  int
	readOnly   bool
}

func (f *structAccessor) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	obj, ok := args[0].(*StructureObject)
	if !ok {
		slip.TypePanic(s, depth, "structure", args[0], f.structName)
	}
	return obj.GetSlotByIndex(f.slotIndex)
}

// Place implements setf support for the accessor.
func (f *structAccessor) Place(s *slip.Scope, args slip.List, value slip.Object) {
	if f.readOnly {
		slip.ErrorPanic(s, 0, "cannot setf read-only slot")
	}
	obj, ok := args[0].(*StructureObject)
	if !ok {
		slip.TypePanic(s, 0, "structure", args[0], f.structName)
	}
	obj.SetSlotByIndex(f.slotIndex, value)
}

// generateTypedFunctions generates functions for :type vector or :type list.
func generateTypedFunctions(s *slip.Scope, sc *StructureClass, depth int) {
	// Calculate offset for :named
	offset := sc.initialOffset
	if sc.named {
		offset++ // First slot holds structure name
	}

	// Generate predicate (only if :named)
	if sc.predicateName != "" && sc.named {
		generateTypedPredicate(sc)
	}

	// Generate constructors
	for _, ctor := range sc.constructors {
		if ctor.boaList != nil {
			generateTypedBOAConstructor(s, sc, ctor.name, ctor.boaList, offset, depth)
		} else {
			generateTypedKeywordConstructor(sc, ctor.name, offset)
		}
	}

	// Generate copier
	if sc.copierName != "" {
		generateTypedCopier(sc)
	}

	// Generate accessors
	for _, slot := range sc.slots {
		actualIndex := offset + slot.index
		if sc.include != nil {
			// Adjust for included slots
			actualIndex = offset + slot.index
		}
		generateTypedAccessor(sc, slot, actualIndex)
	}
}

// generateTypedPredicate creates predicate for typed structures.
func generateTypedPredicate(sc *StructureClass) {
	name := sc.predicateName
	structName := sc.name
	repType := sc.repType

	slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := typedStructPredicate{
				Function:   slip.Function{Name: name, Args: args},
				structName: structName,
				repType:    repType,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   name,
			Args:   []*slip.DocArg{{Name: "object", Type: "t"}},
			Return: "boolean",
			Text:   fmt.Sprintf("Returns t if object is a %s structure.", structName),
		},
	)
}

type typedStructPredicate struct {
	slip.Function
	structName string
	repType    slip.Symbol
}

func (f *typedStructPredicate) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)

	if f.repType == slip.Symbol("vector") {
		if vec, ok := args[0].(*slip.Vector); ok {
			if vec.Length() > 0 {
				if sym, ok := vec.Get(0).(slip.Symbol); ok && string(sym) == f.structName {
					return slip.TrueSymbol
				}
			}
		}
	} else if f.repType == slip.Symbol("list") {
		if list, ok := args[0].(slip.List); ok {
			if len(list) > 0 {
				if sym, ok := list[0].(slip.Symbol); ok && string(sym) == f.structName {
					return slip.TrueSymbol
				}
			}
		}
	}
	return nil
}

// generateTypedKeywordConstructor creates constructor for typed structures.
func generateTypedKeywordConstructor(sc *StructureClass, name string, offset int) {
	structClass := sc
	structName := sc.name
	repType := sc.repType
	named := sc.named
	totalSize := offset + len(sc.slots)

	slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := typedStructConstructor{
				Function:    slip.Function{Name: name, Args: args},
				structClass: structClass,
				structName:  structName,
				repType:     repType,
				named:       named,
				offset:      offset,
				totalSize:   totalSize,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   name,
			Return: string(repType),
			Text:   fmt.Sprintf("Creates a new %s structure as a %s.", structName, repType),
		},
	)
}

type typedStructConstructor struct {
	slip.Function
	structClass *StructureClass
	structName  string
	repType     slip.Symbol
	named       bool
	offset      int
	totalSize   int
}

func (f *typedStructConstructor) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	d2 := depth + 1

	if f.repType == slip.Symbol("vector") {
		vec := slip.NewVector(f.totalSize, slip.TrueSymbol, nil, nil, false)
		if f.named {
			vec.Set(slip.Symbol(f.structName), 0)
		}
		// Initialize with initforms
		for _, slot := range f.structClass.slots {
			if slot.initform != nil {
				vec.Set(s.Eval(slot.initform, d2), f.offset+slot.index)
			}
		}
		// Apply keyword args
		for i := 0; i < len(args)-1; i += 2 {
			if sym, ok := args[i].(slip.Symbol); ok {
				name := string(sym)
				if len(name) > 0 && name[0] == ':' {
					name = name[1:]
				}
				if slot := f.structClass.GetSlot(name); slot != nil {
					vec.Set(args[i+1], f.offset+slot.index)
				}
			}
		}
		return vec
	} else {
		// List representation
		result := make(slip.List, f.totalSize)
		if f.named {
			result[0] = slip.Symbol(f.structName)
		}
		// Initialize with initforms
		for _, slot := range f.structClass.slots {
			if slot.initform != nil {
				result[f.offset+slot.index] = s.Eval(slot.initform, d2)
			}
		}
		// Apply keyword args
		for i := 0; i < len(args)-1; i += 2 {
			if sym, ok := args[i].(slip.Symbol); ok {
				name := string(sym)
				if len(name) > 0 && name[0] == ':' {
					name = name[1:]
				}
				if slot := f.structClass.GetSlot(name); slot != nil {
					result[f.offset+slot.index] = args[i+1]
				}
			}
		}
		return result
	}
}

// generateTypedCopier creates copier for typed structures.
func generateTypedCopier(sc *StructureClass) {
	name := sc.copierName
	repType := sc.repType

	slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := typedStructCopier{
				Function: slip.Function{Name: name, Args: args},
				repType:  repType,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   name,
			Return: string(repType),
			Text:   "Creates a shallow copy of the structure.",
		},
	)
}

type typedStructCopier struct {
	slip.Function
	repType slip.Symbol
}

func (f *typedStructCopier) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)

	if f.repType == slip.Symbol("vector") {
		vec, ok := args[0].(*slip.Vector)
		if !ok {
			slip.TypePanic(s, depth, "vector", args[0], "vector")
		}
		// Create a copy of the vector
		elements := make(slip.List, vec.Length())
		copy(elements, vec.AsList())
		return slip.NewVector(vec.Length(), vec.ElementType(), nil, elements, vec.Adjustable())
	} else {
		list, ok := args[0].(slip.List)
		if !ok {
			slip.TypePanic(s, depth, "list", args[0], "list")
		}
		result := make(slip.List, len(list))
		copy(result, list)
		return result
	}
}

// generateTypedAccessor creates accessor for typed structures.
func generateTypedAccessor(sc *StructureClass, slot *StructureSlot, actualIndex int) {
	name := sc.accessorName(slot.name)
	repType := sc.repType
	readOnly := slot.readOnly

	slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := typedStructAccessor{
				Function:    slip.Function{Name: name, Args: args},
				repType:     repType,
				slotIndex:   actualIndex,
				readOnly:    readOnly,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   name,
			Return: "t",
			Text:   fmt.Sprintf("Returns the %s slot.", slot.name),
		},
	)
}

type typedStructAccessor struct {
	slip.Function
	repType   slip.Symbol
	slotIndex int
	readOnly  bool
}

func (f *typedStructAccessor) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)

	if f.repType == slip.Symbol("vector") {
		vec, ok := args[0].(*slip.Vector)
		if !ok {
			slip.TypePanic(s, depth, "vector", args[0], "vector")
		}
		return vec.Get(f.slotIndex)
	} else {
		list, ok := args[0].(slip.List)
		if !ok {
			slip.TypePanic(s, depth, "list", args[0], "list")
		}
		if f.slotIndex >= len(list) {
			return nil
		}
		return list[f.slotIndex]
	}
}

func (f *typedStructAccessor) Place(s *slip.Scope, args slip.List, value slip.Object) {
	if f.readOnly {
		slip.ErrorPanic(s, 0, "cannot setf read-only slot")
	}

	if f.repType == slip.Symbol("vector") {
		vec, ok := args[0].(*slip.Vector)
		if !ok {
			slip.TypePanic(s, 0, "vector", args[0], "vector")
		}
		vec.Set(value, f.slotIndex)
	} else {
		slip.ErrorPanic(s, 0, "cannot setf list structure slot")
	}
}

// generateBOAConstructor creates a BOA (By Order of Arguments) constructor.
func generateBOAConstructor(s *slip.Scope, sc *StructureClass, name string, boaList slip.List, depth int) {
	structClass := sc

	// Parse the BOA lambda list to determine which slots map to which positions
	// BOA constructors use positional arguments matching the lambda list

	slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := boaConstructor{
				Function:    slip.Function{Name: name, Args: args},
				structClass: structClass,
				boaList:     boaList,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   name,
			Return: sc.name,
			Text:   fmt.Sprintf("Creates a new %s structure using positional arguments.", sc.name),
		},
	)
}

type boaConstructor struct {
	slip.Function
	structClass *StructureClass
	boaList     slip.List
}

func (f *boaConstructor) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := NewStructureObject(f.structClass)
	d2 := depth + 1

	// Initialize all slots with initforms first
	for _, slot := range f.structClass.slots {
		if slot.initform != nil {
			obj.slots[slot.index] = s.Eval(slot.initform, d2)
		}
	}

	// Create a child scope for BOA parameter bindings
	// This allows &aux initforms to reference earlier BOA parameters
	boaScope := s.NewScope()

	// Parse BOA list and apply arguments
	argIndex := 0
	mode := "required" // required, optional, rest, key, aux

	for _, param := range f.boaList {
		switch p := param.(type) {
		case slip.Symbol:
			switch p {
			case slip.Symbol("&optional"):
				mode = "optional"
			case slip.Symbol("&rest"):
				mode = "rest"
			case slip.Symbol("&key"):
				mode = "key"
			case slip.Symbol("&aux"):
				mode = "aux"
			case slip.Symbol("&allow-other-keys"):
				// Ignore
			default:
				slotName := string(p)
				slot := f.structClass.GetSlot(slotName)
				if slot == nil {
					continue
				}

				switch mode {
				case "required":
					if argIndex < len(args) {
						obj.slots[slot.index] = args[argIndex]
						boaScope.Let(p, args[argIndex])
						argIndex++
					}
				case "optional":
					if argIndex < len(args) {
						obj.slots[slot.index] = args[argIndex]
						boaScope.Let(p, args[argIndex])
						argIndex++
					}
				case "rest":
					// Collect remaining args into a list
					if argIndex < len(args) {
						restList := args[argIndex:]
						obj.slots[slot.index] = restList
						boaScope.Let(p, restList)
						argIndex = len(args)
					}
				case "key":
					// Look for keyword in remaining args
					for i := argIndex; i < len(args)-1; i += 2 {
						if sym, ok := args[i].(slip.Symbol); ok {
							keyName := string(sym)
							if len(keyName) > 0 && keyName[0] == ':' {
								keyName = keyName[1:]
							}
							if keyName == slotName {
								obj.slots[slot.index] = args[i+1]
								boaScope.Let(p, args[i+1])
								break
							}
						}
					}
				case "aux":
					// &aux variables use slot's initform (already handled above)
				}
			}

		case slip.List:
			// (var default) or (var default supplied-p)
			if len(p) >= 1 {
				if varSym, ok := p[0].(slip.Symbol); ok {
					slotName := string(varSym)
					slot := f.structClass.GetSlot(slotName)
					if slot == nil {
						continue
					}

					switch mode {
					case "optional":
						if argIndex < len(args) {
							obj.slots[slot.index] = args[argIndex]
							boaScope.Let(varSym, args[argIndex])
							argIndex++
						} else if len(p) >= 2 {
							// Use default from BOA list, evaluated in boaScope
							val := boaScope.Eval(p[1], d2)
							obj.slots[slot.index] = val
							boaScope.Let(varSym, val)
						}
					case "key":
						found := false
						for i := argIndex; i < len(args)-1; i += 2 {
							if sym, ok := args[i].(slip.Symbol); ok {
								keyName := string(sym)
								if len(keyName) > 0 && keyName[0] == ':' {
									keyName = keyName[1:]
								}
								if keyName == slotName {
									obj.slots[slot.index] = args[i+1]
									boaScope.Let(varSym, args[i+1])
									found = true
									break
								}
							}
						}
						if !found && len(p) >= 2 {
							// Use default from BOA list, evaluated in boaScope
							val := boaScope.Eval(p[1], d2)
							obj.slots[slot.index] = val
							boaScope.Let(varSym, val)
						}
					case "aux":
						if len(p) >= 2 {
							// Evaluate in boaScope so it can reference earlier params
							val := boaScope.Eval(p[1], d2)
							obj.slots[slot.index] = val
							boaScope.Let(varSym, val)
						}
					}
				}
			}
		}
	}

	return obj
}

// generateTypedBOAConstructor creates a BOA constructor for typed structures.
func generateTypedBOAConstructor(s *slip.Scope, sc *StructureClass, name string, boaList slip.List, offset int, depth int) {
	structClass := sc
	structName := sc.name
	repType := sc.repType
	named := sc.named
	totalSize := offset + len(sc.slots)

	slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := typedBOAConstructor{
				Function:    slip.Function{Name: name, Args: args},
				structClass: structClass,
				structName:  structName,
				repType:     repType,
				named:       named,
				offset:      offset,
				totalSize:   totalSize,
				boaList:     boaList,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   name,
			Return: string(repType),
			Text:   fmt.Sprintf("Creates a new %s structure using positional arguments.", structName),
		},
	)
}

type typedBOAConstructor struct {
	slip.Function
	structClass *StructureClass
	structName  string
	repType     slip.Symbol
	named       bool
	offset      int
	totalSize   int
	boaList     slip.List
}

func (f *typedBOAConstructor) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	d2 := depth + 1

	// Create the underlying representation
	var slots []slip.Object
	if f.repType == slip.Symbol("vector") {
		vec := slip.NewVector(f.totalSize, slip.TrueSymbol, nil, nil, false)
		if f.named {
			vec.Set(slip.Symbol(f.structName), 0)
		}
		// Initialize with initforms
		for _, slot := range f.structClass.slots {
			if slot.initform != nil {
				vec.Set(s.Eval(slot.initform, d2), f.offset+slot.index)
			}
		}
		// Process BOA arguments (same logic as standard BOA)
		f.applyBOAArgs(s, args, func(slotIndex int, value slip.Object) {
			vec.Set(value, f.offset+slotIndex)
		}, d2)
		return vec
	} else {
		slots = make(slip.List, f.totalSize)
		if f.named {
			slots[0] = slip.Symbol(f.structName)
		}
		// Initialize with initforms
		for _, slot := range f.structClass.slots {
			if slot.initform != nil {
				slots[f.offset+slot.index] = s.Eval(slot.initform, d2)
			}
		}
		// Process BOA arguments
		f.applyBOAArgs(s, args, func(slotIndex int, value slip.Object) {
			slots[f.offset+slotIndex] = value
		}, d2)
		return slip.List(slots)
	}
}

func (f *typedBOAConstructor) applyBOAArgs(s *slip.Scope, args slip.List, setter func(int, slip.Object), depth int) {
	// Create a child scope for BOA parameter bindings
	// This allows &aux initforms to reference earlier BOA parameters
	boaScope := s.NewScope()

	argIndex := 0
	mode := "required"

	for _, param := range f.boaList {
		switch p := param.(type) {
		case slip.Symbol:
			switch p {
			case slip.Symbol("&optional"):
				mode = "optional"
			case slip.Symbol("&rest"):
				mode = "rest"
			case slip.Symbol("&key"):
				mode = "key"
			case slip.Symbol("&aux"):
				mode = "aux"
			case slip.Symbol("&allow-other-keys"):
				// Ignore
			default:
				slot := f.structClass.GetSlot(string(p))
				if slot == nil {
					continue
				}
				switch mode {
				case "required", "optional":
					if argIndex < len(args) {
						setter(slot.index, args[argIndex])
						boaScope.Let(p, args[argIndex])
						argIndex++
					}
				case "rest":
					if argIndex < len(args) {
						restList := args[argIndex:]
						setter(slot.index, restList)
						boaScope.Let(p, restList)
						argIndex = len(args)
					}
				case "key":
					for i := argIndex; i < len(args)-1; i += 2 {
						if sym, ok := args[i].(slip.Symbol); ok {
							keyName := string(sym)
							if len(keyName) > 0 && keyName[0] == ':' {
								keyName = keyName[1:]
							}
							if keyName == string(p) {
								setter(slot.index, args[i+1])
								boaScope.Let(p, args[i+1])
								break
							}
						}
					}
				case "aux":
					// &aux variables use slot's initform (already handled in Call)
				}
			}

		case slip.List:
			if len(p) >= 1 {
				if varSym, ok := p[0].(slip.Symbol); ok {
					slot := f.structClass.GetSlot(string(varSym))
					if slot == nil {
						continue
					}
					switch mode {
					case "optional":
						if argIndex < len(args) {
							setter(slot.index, args[argIndex])
							boaScope.Let(varSym, args[argIndex])
							argIndex++
						} else if len(p) >= 2 {
							// Evaluate in boaScope so it can reference earlier params
							val := boaScope.Eval(p[1], depth)
							setter(slot.index, val)
							boaScope.Let(varSym, val)
						}
					case "key":
						found := false
						for i := argIndex; i < len(args)-1; i += 2 {
							if sym, ok := args[i].(slip.Symbol); ok {
								keyName := string(sym)
								if len(keyName) > 0 && keyName[0] == ':' {
									keyName = keyName[1:]
								}
								if keyName == string(varSym) {
									setter(slot.index, args[i+1])
									boaScope.Let(varSym, args[i+1])
									found = true
									break
								}
							}
						}
						if !found && len(p) >= 2 {
							// Evaluate in boaScope so it can reference earlier params
							val := boaScope.Eval(p[1], depth)
							setter(slot.index, val)
							boaScope.Let(varSym, val)
						}
					case "aux":
						if len(p) >= 2 {
							// Evaluate in boaScope so it can reference earlier params
							val := boaScope.Eval(p[1], depth)
							setter(slot.index, val)
							boaScope.Let(varSym, val)
						}
					}
				}
			}
		}
	}
}
