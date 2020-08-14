package main

import (
	"fmt"

	"github.com/rychipman/mongosql-rs-example/mongosql"
)

func main() {
	tr := mongosql.Translate("select * from foo join bar")
	fmt.Printf("got translation: %s\n", tr)
}
