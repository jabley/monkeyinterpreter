package ast

// ModifierFunc is the type used to replace a Node with a different Node when
// walking the AST.
type ModifierFunc func(Node) Node

// Modify recursively walks the AST and calls the ModifierFunc, returning the
// result. This allows us to replace nodes in the AST.
func Modify(node Node, modifier ModifierFunc) Node {
	switch node := node.(type) {

	case *Program:
		for i, statement := range node.Statements {
			node.Statements[i], _ = Modify(statement, modifier).(Statement)
		}
	case *ExpressionStatement:
		node.Expression, _ = Modify(node.Expression, modifier).(Expression)
	case *InfixExpression:
		node.Left, _ = Modify(node.Left, modifier).(Expression)
		node.Right, _ = Modify(node.Right, modifier).(Expression)
	}

	return modifier(node)
}
