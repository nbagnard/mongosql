package mongosql

/*
#cgo LDFLAGS: -L../../target/debug/ -lmongosql -ldl -lm
#include "./mongosql.h"
*/
import "C"
import "fmt"

func Translate() {
	sql := C.CString("select * from foo join bar")
	C.translate(sql)
	output := C.GoString(C.translate(sql))
	fmt.Printf("got output: %s\n", output)
}
