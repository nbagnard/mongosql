package mongosql

/*
#cgo LDFLAGS: -L../../target/debug/ -lmongosql -ldl -lm
#include <stdlib.h>
#include "./mongosql.h"
*/
import "C"
import (
	"unsafe"
)

// Translate takes a SQL string and returns an extJSON string
// representation of its agg-pipeline translation.
func Translate(sql string) string {
	cSQL := C.CString(sql)

	cTranslation := C.translate(cSQL)
	translation := C.GoString(cTranslation)

	C.free(unsafe.Pointer(cSQL))
	C.free(unsafe.Pointer(cTranslation))

	return translation
}
