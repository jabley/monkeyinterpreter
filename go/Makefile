GOPATH:=${CURDIR}

all: test build

test:
	go vet ./...
	go test -coverprofile=monkey.coverage ./...

build:
	go build -o ./bin/monkey monkey/

cover: test
	go tool cover -html=monkey.coverage

clean:
	rm -rf ./bin/
