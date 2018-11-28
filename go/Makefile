GOPATH:=${CURDIR}

all: test build

test:
	go vet ./...
	go test -v ./...

build:
	go build -o ./bin/monkey monkey/

clean:
	rm -rf ./bin/
