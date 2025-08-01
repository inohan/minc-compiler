package main
import "os"

/* read XML file and convert it to assembly string */
func file_xml_to_asm(file_xml string) string {
	program := file_xml_to_ast(file_xml) // in minc_parse.go
	asm := ast_to_asm_program(program)	     // in minc_cogen.go
	return asm
}

/* read XML file, convert it to assembly string, and write
   it to a file (file_asm) */
func file_xml_to_file_asm(file_xml string, file_asm string) {
	asm := file_xml_to_asm(file_xml)
	os.WriteFile(file_asm, []byte(asm), 0644)
}

/* entry point 
   ./minc fun.xml fun.s 
   read an XML file and generate assembly code in fun.s
*/
func main() {
	file_xml := os.Args[1]
	file_asm := os.Args[2]
	file_xml_to_file_asm(file_xml, file_asm)
}
