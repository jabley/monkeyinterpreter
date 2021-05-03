Go version (as per the books) of https://interpreterbook.com/

Also includes [the macro system](https://interpreterbook.com/#the-lost-chapter).

We have [a tree-walking interpreter version](./evaluator/evaluator.go), and [a stack-based VM version](./compiler/compiler.go). The VM is ~4 times faster than the interpreter.

    > make bench
    go test -bench Bench ./...
    goos: darwin
    goarch: amd64
    pkg: github.com/jabley/monkey/benchmark
    BenchmarkEvaluator-8   	       1	19599206521 ns/op
    BenchmarkVM-8          	       1	5669863576 ns/op


The Go version is also the fastest to date:

    > time go run bin/vm-bench.go
    9227465

    real	0m5.958s
    user	0m6.275s
    sys     0m0.348s

The [Makefile](Makefile) is useful.

## Profiling

Since Go 1.11, you have flamegraphs available too.

    go test -bench BenchmarkV -cpuprofile=cpuprof_vm.out github.com/jabley/monkey/benchmark
    go tool pprof -http=":8081" cpuprof_vm.out
