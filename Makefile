
rust:
	cargo build

golang:
	cd go/mongosql && go build .

build: rust golang

run: build
	./go/mongosql/mongosql

clean:
	rm -f go/mongosql/mongosql; rm -rf target/
