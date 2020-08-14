package main

import (
	"fmt"

	"github.com/rychipman/mongosql-rs-example/mongosql"
)

func main() {
	v := mongosql.Version()
	fmt.Printf("version: %s\n", v)
	tr := mongosql.Translate("select * from foo join bar")
	fmt.Printf("got translation: %s\n", tr)
}
