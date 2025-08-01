package main

import "fmt"

type Inst interface {
	ToASM() string
}

type InstValue interface {
	ValueString() string
}

type InstValueReg struct {
	Name string
}

func (v *InstValueReg) ValueString() string {
	return v.Name
}

type InstValueInt struct {
	Value int
}

type InstValueFloat struct {
	Value float32
}

type InstValueDouble struct {
	Value float64
}

func (v *InstValueInt) ValueString() string {
	return fmt.Sprintf("%d", v.Value)
}

func (v *InstValueFloat) ValueString() string {
	return fmt.Sprintf("#%f", v.Value)
}

func (v *InstValueDouble) ValueString() string {
	return fmt.Sprintf("#%f", v.Value)
}

type InstLabel struct {
	Name string
}

func (i *InstLabel) ToASM() string {
	return i.Name + ":"
}

type InstAdd struct {
	Res *InstValueReg
	I0  InstValue
	I1  InstValue
}

func (i *InstAdd) ToASM() string {
	regType, ok := MatchRegisterType(i.Res, i.I0, i.I1)
	if !ok {
		panic(fmt.Sprintf("Cannot match register type for %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString()))
	}
	switch regType {
	case "double", "float":
		return fmt.Sprintf("\tfadd %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
	case "long", "int":
		return fmt.Sprintf("\tadd %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
	}
	panic(fmt.Sprintf("Unknown register type: %s", regType))
}

type InstSub struct {
	Res *InstValueReg
	I0  InstValue
	I1  InstValue
}

func (i *InstSub) ToASM() string {
	regType, ok := MatchRegisterType(i.Res, i.I0, i.I1)
	if !ok {
		panic(fmt.Sprintf("Cannot match register type for %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString()))
	}
	switch regType {
	case "double", "float":
		return fmt.Sprintf("\tfsub %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
	case "long", "int":
		return fmt.Sprintf("\tsub %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
	}
	panic(fmt.Sprintf("Unknown register type: %s", regType))
}

type InstMul struct {
	Res *InstValueReg
	I0  InstValue
	I1  InstValue
}

func (i *InstMul) ToASM() string {
	regType, ok := MatchRegisterType(i.Res, i.I0, i.I1)
	if !ok {
		panic(fmt.Sprintf("Cannot match register type for %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString()))
	}
	switch regType {
	case "double", "float":
		return fmt.Sprintf("\tfmul %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
	case "long", "int":
		return fmt.Sprintf("\tmul %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
	}
	panic(fmt.Sprintf("Unknown register type: %s", regType))
}

// res = i2 + i0 * i1
type InstMAdd struct {
	Res *InstValueReg
	I0  InstValue
	I1  InstValue
	I2  InstValue
}

func (i *InstMAdd) ToASM() string {
	regType, ok := MatchRegisterType(i.Res, i.I0, i.I1, i.I2)
	if !ok {
		panic(fmt.Sprintf("Cannot match register type for %s, %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString(), i.I2.ValueString()))
	}
	switch regType {
	case "double", "float":
		return fmt.Sprintf("\tfmadd %s, %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString(), i.I2.ValueString())
	case "long", "int":
		return fmt.Sprintf("\tmadd %s, %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString(), i.I2.ValueString())
	}
	panic(fmt.Sprintf("Unknown register type: %s", regType))
}

// res = i2 - i0 * i1
type InstMSub struct {
	Res *InstValueReg
	I0  InstValue
	I1  InstValue
	I2  InstValue
}

func (i *InstMSub) ToASM() string {
	regType, ok := MatchRegisterType(i.Res, i.I0, i.I1, i.I2)
	if !ok {
		panic(fmt.Sprintf("Cannot match register type for %s, %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString(), i.I2.ValueString()))
	}
	switch regType {
	case "double", "float":
		return fmt.Sprintf("\tfmsub %s, %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString(), i.I2.ValueString())
	case "long", "int":
		return fmt.Sprintf("\tmsub %s, %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString(), i.I2.ValueString())
	}
	panic(fmt.Sprintf("Unknown register type: %s", regType))
}

type InstSdiv struct {
	Res *InstValueReg
	I0  InstValue
	I1  InstValue
}

func (i *InstSdiv) ToASM() string {
	regType, ok := MatchRegisterType(i.Res, i.I0, i.I1)
	if !ok {
		panic(fmt.Sprintf("Cannot match register type for %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString()))
	}
	switch regType {
	case "double", "float":
		return fmt.Sprintf("\tfdiv %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
	case "long", "int":
		return fmt.Sprintf("\tsdiv %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
	}
	panic(fmt.Sprintf("Unknown register type: %s", regType))
}

// And
type InstAnd struct {
	Res *InstValueReg
	I0  InstValue
	I1  InstValue
}

func (i *InstAnd) ToASM() string {
	return fmt.Sprintf("\tand %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
}

// Or
type InstOrr struct {
	Res *InstValueReg
	I0  InstValue
	I1  InstValue
}

func (i *InstOrr) ToASM() string {
	return fmt.Sprintf("\torr %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
}

// Xor
type InstEor struct {
	Res *InstValueReg
	I0  InstValue
	I1  InstValue
}

func (i *InstEor) ToASM() string {
	return fmt.Sprintf("\teor %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
}

// Arithmetic shift right
type InstAsr struct {
	Res *InstValueReg
	I0  InstValue
	I1  InstValue
}

func (i *InstAsr) ToASM() string {
	return fmt.Sprintf("\tasr %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
}

// Logical shift left
type InstLsl struct {
	Res *InstValueReg
	I0  InstValue
	I1  InstValue
}

func (i *InstLsl) ToASM() string {
	return fmt.Sprintf("\tlsl %s, %s, %s", i.Res.ValueString(), i.I0.ValueString(), i.I1.ValueString())
}

type InstNeg struct {
	Res *InstValueReg
	I   InstValue
}

func (i *InstNeg) ToASM() string {
	regType, ok := MatchRegisterType(i.Res, i.I)
	if !ok {
		panic(fmt.Sprintf("Cannot match register type for %s, %s", i.Res.ValueString(), i.I.ValueString()))
	}
	switch regType {
	case "double", "float":
		return fmt.Sprintf("\tfneg %s, %s", i.Res.ValueString(), i.I.ValueString())
	case "long", "int":
		return fmt.Sprintf("\tneg %s, %s", i.Res.ValueString(), i.I.ValueString())
	}
	panic(fmt.Sprintf("Unknown register type: %s", regType))
}

type InstRet struct {
}

func (i *InstRet) ToASM() string {
	return "\tret"
}

type InstB struct {
	Label *InstLabel
}

func (i *InstB) ToASM() string {
	return fmt.Sprintf("\tb %s", i.Label.Name)
}

type InstBne struct {
	Label *InstLabel
}

func (i *InstBne) ToASM() string {
	return fmt.Sprintf("\tb.ne %s", i.Label.Name)
}

type InstBeq struct {
	Label *InstLabel
}

func (i *InstBeq) ToASM() string {
	return fmt.Sprintf("\tb.eq %s", i.Label.Name)
}

type InstBl struct {
	Name string
}

func (i *InstBl) ToASM() string {
	return fmt.Sprintf("\tbl %s", i.Name)
}

type InstMov struct {
	dest *InstValueReg
	src  InstValue
}

func (i *InstMov) ToASM() string {
	regType, ok := MatchRegisterType(i.dest)
	if !ok {
		panic(fmt.Sprintf("Cannot match register type for %s, %s", i.dest.ValueString(), i.src.ValueString()))
	}
	switch regType {
	case "double", "float":
		return fmt.Sprintf("\tfmov %s, %s", i.dest.ValueString(), i.src.ValueString())
	case "long", "int":
		return fmt.Sprintf("\tmov %s, %s", i.dest.ValueString(), i.src.ValueString())
	}
	panic(fmt.Sprintf("Unknown register type: %s", regType))
}

type InstMvn struct {
	dest *InstValueReg
	src  InstValue
}

func (i *InstMvn) ToASM() string {
	return fmt.Sprintf("\tmvn %s, %s", i.dest.ValueString(), i.src.ValueString())
}

// Sign extend word
type InstSxtw struct {
	dest *InstValueReg
	src  *InstValueReg
}

func (i *InstSxtw) ToASM() string {
	return fmt.Sprintf("\tsxtw %s, %s", i.dest.ValueString(), i.src.ValueString())
}

// Sign extend half-word
type InstSxth struct {
	dest *InstValueReg
	src  *InstValueReg
}

func (i *InstSxth) ToASM() string {
	return fmt.Sprintf("\tsxth %s, %s", i.dest.ValueString(), i.src.ValueString())
}

// Sign extend byte
type InstSxtb struct {
	dest *InstValueReg
	src  *InstValueReg
}

func (i *InstSxtb) ToASM() string {
	return fmt.Sprintf("\tsxtb %s, %s", i.dest.ValueString(), i.src.ValueString())
}

// Signed fixed-point convert to floating point
type InstScvtf struct {
	dest *InstValueReg
	src  *InstValueReg
}

func (i *InstScvtf) ToASM() string {
	return fmt.Sprintf("\tscvtf %s, %s", i.dest.ValueString(), i.src.ValueString())
}

// Floating point convert to integer
type InstFcvtzs struct {
	dest *InstValueReg
	src  *InstValueReg
}

func (i *InstFcvtzs) ToASM() string {
	return fmt.Sprintf("\tfcvtzs %s, %s", i.dest.ValueString(), i.src.ValueString())
}

// Floating point convert
type InstFcvt struct {
	dest *InstValueReg
	src  *InstValueReg
}

func (i *InstFcvt) ToASM() string {
	return fmt.Sprintf("\tfcvt %s, %s", i.dest.ValueString(), i.src.ValueString())
}

type InstMemoryAddr interface {
	AddrString() string
}

// Shift base register after I/O
type InstMemoryPostIndex struct {
	BaseReg *InstValueReg
	Offset  int
}

func (i *InstMemoryPostIndex) AddrString() string {
	if i.Offset == 0 {
		return fmt.Sprintf("[%s]", i.BaseReg.ValueString())
	}
	return fmt.Sprintf("[%s], %d", i.BaseReg.ValueString(), i.Offset)
}

type InstMemoryPreIndex struct {
	BaseReg *InstValueReg
	Offset  int
}

func (i *InstMemoryPreIndex) AddrString() string {
	if i.Offset == 0 {
		return fmt.Sprintf("[%s]", i.BaseReg.ValueString())
	}
	return fmt.Sprintf("[%s, %d]!", i.BaseReg.ValueString(), i.Offset)
}

type InstMemoryOffset struct {
	BaseReg *InstValueReg
	Offset  int
}

func (i *InstMemoryOffset) AddrString() string {
	if i.Offset == 0 {
		return fmt.Sprintf("[%s]", i.BaseReg.ValueString())
	}
	return fmt.Sprintf("[%s, %d]", i.BaseReg.ValueString(), i.Offset)
}

type InstMemorySpOffset struct {
	Offset int
}

func (i *InstMemorySpOffset) AddrString() string {
	offset := &InstMemoryOffset{&InstValueReg{"sp"}, i.Offset}
	return offset.AddrString()
}

type InstMemoryFrameEndOffset struct {
	Offset         int
	StackFrameSize *int
}

func (i *InstMemoryFrameEndOffset) AddrString() string {
	offset := &InstMemoryOffset{&InstValueReg{"sp"}, Align(*i.StackFrameSize, 16) + i.Offset}
	return offset.AddrString()
}

type InstMemoryPool struct {
	Int    int
	Float  float32
	Double float64
}

func (i *InstMemoryPool) AddrString() string {
	if i.Int != 0 {
		return fmt.Sprintf("=%d", i.Int)
	}
	if i.Float != 0 {
		return fmt.Sprintf("=%f", i.Float)
	}
	return fmt.Sprintf("=%f", i.Double)
}

type InstStr struct {
	Reg        *InstValueReg
	MemoryAddr InstMemoryAddr
}

func (i *InstStr) ToASM() string {
	return fmt.Sprintf("\tstr %s, %s", i.Reg.ValueString(), i.MemoryAddr.AddrString())
}

type InstStp struct {
	Reg1       *InstValueReg
	Reg2       *InstValueReg
	MemoryAddr InstMemoryAddr
}

func (i *InstStp) ToASM() string {
	return fmt.Sprintf("\tstp %s, %s, %s", i.Reg1.ValueString(), i.Reg2.ValueString(), i.MemoryAddr.AddrString())
}

type InstLdr struct {
	Reg        *InstValueReg
	MemoryAddr InstMemoryAddr
}

func (i *InstLdr) ToASM() string {
	return fmt.Sprintf("\tldr %s, %s", i.Reg.ValueString(), i.MemoryAddr.AddrString())
}

type InstLdp struct {
	Reg1       *InstValueReg
	Reg2       *InstValueReg
	MemoryAddr InstMemoryAddr
}

func (i *InstLdp) ToASM() string {
	return fmt.Sprintf("\tldp %s, %s, %s", i.Reg1.ValueString(), i.Reg2.ValueString(), i.MemoryAddr.AddrString())
}

type InstCmp struct {
	I0 InstValue
	I1 InstValue
}

func (i *InstCmp) ToASM() string {
	regType, ok := MatchRegisterType(i.I0, i.I1)
	if !ok {
		panic(fmt.Sprintf("Cannot match register type for %s, %s", i.I0.ValueString(), i.I1.ValueString()))
	}
	switch regType {
	case "double", "float":
		return fmt.Sprintf("\tfcmp %s, %s", i.I0.ValueString(), i.I1.ValueString())
	case "long", "int":
		return fmt.Sprintf("\tcmp %s, %s", i.I0.ValueString(), i.I1.ValueString())
	}
	panic(fmt.Sprintf("Unknown register type: %s", regType))
}

type InstCset struct {
	Reg *InstValueReg
	Op  string
}

func (i *InstCset) ToASM() string {
	regType, ok := MatchRegisterType(i.Reg)
	if !ok || regType == "double" || regType == "float" {
		panic(fmt.Sprintf("Cannot match register type for %s", i.Reg.ValueString()))
	}
	return fmt.Sprintf("\tcset %s, %s", i.Reg.ValueString(), i.Op)
}

func MatchRegisterType(regs ...InstValue) (string, bool) {
	regTypeCommon := ""
	for _, regValue := range regs {
		reg, ok := regValue.(*InstValueReg)
		if !ok {
			continue
		}
		var regType string
		if reg.Name == "sp" {
			regType = "x"
		} else {
			regType = string(reg.Name[0])
		}
		if regTypeCommon == "" {
			regTypeCommon = regType
		}
		if regType != regTypeCommon {
			return "", false
		}
	}
	switch regTypeCommon {
	case "d":
		return "double", true
	case "s":
		return "float", true
	case "x":
		return "long", true
	case "w":
		return "int", true
	}
	return "", false
}
