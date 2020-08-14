
rust:
	cargo build

golang:
	cd go && go build -o mongosqlrun .

build: rust golang

run: build
	./go/mongosqlrun

clean:
	rm -f go/mongosql/mongosql; rm -rf target/
