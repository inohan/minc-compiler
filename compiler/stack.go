package main

type StackManager struct {
	CurrentStack int
	MaxStack *int
}

func (s *StackManager) SetCurrent(addr int) {
	s.CurrentStack = addr
	if s.CurrentStack > *s.MaxStack {
		*s.MaxStack = s.CurrentStack
	}
}

func (s *StackManager) Align(size int) int {
	s.SetCurrent(Align(s.CurrentStack, size))
	return s.CurrentStack
}

func (s *StackManager) ExtendCustom(typeSize int, typeAlignment int) int {
	newAddr := Align(s.CurrentStack, typeAlignment)
	s.SetCurrent(newAddr + typeSize)
	return newAddr
}

func (s *StackManager) Extend(objectType TypeExpr) int {
	// Align memory address
	typeSize := GetTypeSize(objectType)
	typeAlignment := GetNaturalAlignment(objectType)
	return s.ExtendCustom(typeSize, typeAlignment)
}