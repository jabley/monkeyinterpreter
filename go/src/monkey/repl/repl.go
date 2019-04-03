package repl

import (
	"bufio"
	"fmt"
	"io"
	"monkey/compiler"
	"monkey/evaluator"
	"monkey/lexer"
	"monkey/object"
	"monkey/parser"
	"monkey/vm"
)

const prompt = ">> "

// Start the loop of the REPL
func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)
	// env := object.NewEnvironment()
	macroEnv := object.NewEnvironment()
	for {
		fmt.Fprintf(out, prompt)
		scanned := scanner.Scan()

		if !scanned {
			return
		}

		line := scanner.Text()

		l := lexer.New(line)
		p := parser.New(l)

		program := p.ParseProgram()

		if len(p.Errors()) != 0 {
			printParserErrors(out, p.Errors())
			continue
		}

		evaluator.DefineMacros(program, macroEnv)
		expanded := evaluator.ExpandMacros(program, macroEnv)

		comp := compiler.New()
		err := comp.Compile(expanded)
		if err != nil {
			io.WriteString(out, fmt.Sprintf("Whoops! Compilation failed:\n%s\n", err))
			continue
		}

		machine := vm.New(comp.Bytecode())
		err = machine.Run()
		if err != nil {
			io.WriteString(out, fmt.Sprintf("Whoops! Executing bytecode failed:\n%s\n", err))
			continue
		}

		stackTop := machine.StackTop()
		if stackTop != nil {
			io.WriteString(out, stackTop.Inspect())
			io.WriteString(out, "\n")
		}
	}
}

const monkeyFace = `            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
`

func printParserErrors(out io.Writer, errors []string) {
	io.WriteString(out, monkeyFace)
	io.WriteString(out, "Woops! We ran into some monkey business here!\n")
	io.WriteString(out, " parser errors:\n")

	for _, msg := range errors {
		io.WriteString(out, fmt.Sprintf("\t%s\n", msg))
	}
}
