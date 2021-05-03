Rust implementation of [Monkey](https://interpreterbook.com/).

As per the Go original, we have a [tree-walking interpreter version](./src/evaluator.rs), and a
[stack-based VM version](./src/vm/mod.rs).

The interpreter is around 4 times slower than the VM version.

    > cargo bench
        Finished bench [optimized] target(s) in 0.14s
         Running target/release/deps/my_benchmark-fc933bad66fbb0f8
    fib 18 (Interpreter)    time:   [14.487 ms 14.567 ms 14.658 ms]

    fib 18 (VM)             time:   [2.7717 ms 2.7843 ms 2.7975 ms]

And the Rust VM is [slightly slower than the Go VM](../../go/src/README.md).

    > time ./target/release/vm-flamegraph
    Result: 9227465

    real    0m9.258s
    user    0m9.130s
    sys     0m0.028s

I would put this down to various things:

* this is my first Rust program of any size. 5k lines of Rust – I'm still learning :)
* this is a direct port of the Go version to Rust. Some Go idioms will not translate well. For
  example, the Go version does not need to copy Object implementations. `vm.push` will push the
  pointer. In Rust, ownership rules mean that with the current design, we need to clone constant
  Objects when pushing them on to the stack. A performance improvement in the Rust
  implementation might mean that `vm.pop()` will return an owned Object, rather than a borrowed
  one?

It's been a fascinating exercise. I've not yet found many other Rust-based stack VMs to compare approaches and learn from.

## Profiling

### Flamegraphs

    cargo install flamegraph
    cargo flamegraph --root --bin vm-flamegraph

This will generate a flamegraph based on the recursive fibonacci program for the 35th fibonacci number. Useful to see where time is going, and was the thing that guided improvements which gave a 4-fold improvement in execution time.

### Instruments (on MacOS)

Instruments is the profiling toolset in Xcode.

    cargo install cargo-instruments
    cargo instruments --bin vm-flamegraph --open

This takes longer to capture the data, but gave another very fine-grained view of CPU time for `fib(35)`.

### CLion with Rust plugin

Running the profiler in CLion gave a much more detailed flamegraph.
See [flamegraphs](flamegraphs/) for the exports.

We can use the exported trace to re-create a flamegraph in other tooling such as [inferno](https://github.com/jonhoo/inferno).

    cargo install inferno
    cat flamegraphs/af1dadfcda_vm-flamegraph_2021-05-03-012730.collapsed | inferno-flamegraph > flamegraph.svg

This showed that for the version af1dadfcda, the time was being spent in:

* ~15% in `VM::call_function`
* ~15% in `VM::get_global`

Both of those were spending lots of time in `object::Object::clone`, which were respectively spending time in `Vec::clone`.
So that's a lot of memory copying happening.
Investigating how to minimise that memory copying led to the adoption of `std::rc::Rc` to use Rust's idiom for shared immutable memory.
This gave a different profile of where time was being spent, and much better performance in the interpreter and VM.
Turns out incrementing and decrementing a reference count is much faster than copying bytes.
Who knew!