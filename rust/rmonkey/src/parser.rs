use crate::ast::{Expression, InfixOperator, PrefixOperator, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Precedence {
    Lowest,
    Equals,      // == or !=
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // array[index]
}

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    ExpectedAssign(Token),
    ExpectedBooleanToken(Token),
    ExpectedIdentifierToken(Token),
    ExpectedInfixToken(Token),
    ExpectedIntegerToken(Token),
    ExpectedPrefixToken(Token),
}

type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Parser {
            lexer,
            cur_token: Token::Eof,
            peek_token: Token::Eof,
            errors: vec![],
        };

        // Read 2 tokens so that cur_token and peek_token are both initialised
        p.next_token();
        p.next_token();

        p
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = vec![];

        while self.cur_token != Token::Eof {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => self.errors.push(err),
            }
            self.next_token();
        }

        Program { statements }
    }

    pub fn errors(&self) -> &[ParserError] {
        &self.errors
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        let name;

        if let Token::Ident(ident) = self.peek_token.clone() {
            self.next_token();
            name = ident;
        } else {
            return Err(ParserError::ExpectedIdentifierToken(
                self.peek_token.clone(),
            ));
        }

        self.expect_peek(Token::Assign, ParserError::ExpectedAssign)?;
        self.next_token();

        // TODO(jabley): parse the expression properly.
        while self.cur_token != Token::SemiColon {
            self.next_token();
        }

        Ok(Statement::Let(name))
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.next_token();

        // TODO(jabley): parse the expression properly.
        while self.cur_token != Token::SemiColon {
            self.next_token();
        }

        Ok(Statement::Return)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Token::SemiColon {
            self.next_token();
        }

        expression.map(Statement::Expression)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let mut left = self
            .prefix_parse()
            .or_else(|_| Err(ParserError::ExpectedPrefixToken(self.cur_token.clone())))?;

        while self.peek_token != Token::SemiColon
            && precedence < self.infix_token(&self.peek_token).0
        {
            if let Some(infix) = self.infix_parse_fn() {
                self.next_token();
                left = infix(self, left)?;
            } else {
                return Ok(left);
            }
        }

        Ok(left)
    }

    fn prefix_parse(&mut self) -> Result<Expression> {
        match self.cur_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_integer(),
            Token::Bang | Token::Minus => self.parse_prefix(),
            Token::True | Token::False => self.parse_boolean(),
            _ => unimplemented!("{}", self.cur_token),
        }
    }

    fn parse_boolean(&self) -> Result<Expression> {
        match self.cur_token {
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            _ => Err(ParserError::ExpectedBooleanToken(self.cur_token.clone())),
        }
    }

    fn parse_identifier(&self) -> Result<Expression> {
        self.parse_identifier_string().map(Expression::Identifier)
    }

    fn parse_identifier_string(&self) -> Result<String> {
        if let Token::Ident(ident) = &self.cur_token {
            Ok(ident.to_string())
        } else {
            Err(ParserError::ExpectedIdentifierToken(self.cur_token.clone()))
        }
    }

    fn parse_integer(&self) -> Result<Expression> {
        if let Token::Int(i) = self.cur_token {
            Ok(Expression::Integer(i))
        } else {
            Err(ParserError::ExpectedIntegerToken(self.cur_token.clone()))
        }
    }

    fn parse_prefix(&mut self) -> Result<Expression> {
        let operator = self.parse_prefix_token()?;
        self.next_token();
        let expression = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix(operator, Box::new(expression)))
    }

    fn parse_prefix_token(&self) -> Result<PrefixOperator> {
        match self.cur_token {
            Token::Bang => Ok(PrefixOperator::Bang),
            Token::Minus => Ok(PrefixOperator::Minus),
            _ => Err(ParserError::ExpectedPrefixToken(self.cur_token.clone())),
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let (precedence, infix) = self.infix_token(&self.cur_token);
        let i = infix.ok_or_else(|| ParserError::ExpectedInfixToken(self.cur_token.clone()))?;
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(i, Box::new(left), Box::new(right)))
    }

    fn infix_token(&self, token: &Token) -> (Precedence, Option<InfixOperator>) {
        match token {
            Token::Eq => (Precedence::Equals, Some(InfixOperator::Eq)),
            Token::Ne => (Precedence::Equals, Some(InfixOperator::NotEq)),
            Token::Lt => (Precedence::LessGreater, Some(InfixOperator::Lt)),
            Token::Gt => (Precedence::LessGreater, Some(InfixOperator::Gt)),
            Token::Plus => (Precedence::Sum, Some(InfixOperator::Plus)),
            Token::Minus => (Precedence::Sum, Some(InfixOperator::Minus)),
            Token::Slash => (Precedence::Product, Some(InfixOperator::Slash)),
            Token::Asterisk => (Precedence::Product, Some(InfixOperator::Asterisk)),
            _ => (Precedence::Lowest, None),
        }
    }

    fn infix_parse_fn(&mut self) -> Option<InfixParseFn> {
        match &self.peek_token {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Eq
            | Token::Ne
            | Token::Lt
            | Token::Gt => Some(|parser, left| parser.parse_infix_expression(left)),
            _ => None,
        }
    }

    fn expect_peek(&mut self, token: Token, expected: fn(Token) -> ParserError) -> Result<()> {
        if self.peek_token != token {
            return Err(expected(self.peek_token.clone()));
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::ast::{Expression, InfixOperator, PrefixOperator, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn let_statement() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            3,
            program.statements.len(),
            "Parser errors: {:?}",
            parser.errors
        );

        assert_eq!(
            program.statements,
            vec![
                Statement::Let("x".to_string()),
                Statement::Let("y".to_string()),
                Statement::Let("foobar".to_string()),
            ]
        );
    }

    #[test]
    fn return_statement() {
        let input = "
return 5;
return 10;
return 993322;
";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            3,
            program.statements.len(),
            "Parser errors: {:?}",
            parser.errors
        );

        assert_eq!(
            program.statements,
            vec![Statement::Return, Statement::Return, Statement::Return,]
        );
    }

    #[test]
    fn expression_statement() {
        let input = "
foobar;
";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            1,
            program.statements.len(),
            "Parser errors: {:?}",
            parser.errors
        );

        assert_eq!(
            Statement::Expression(Expression::Identifier("foobar".to_owned())),
            program.statements[0],
        );
    }

    #[test]
    fn integer_literals() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            1,
            program.statements.len(),
            "Parser errors: {:?}",
            parser.errors
        );

        assert_eq!(
            Statement::Expression(Expression::Integer(5)),
            program.statements[0],
        );
    }

    #[test]
    fn prefix_expressions() {
        let input = "
-5;
!15;
";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(
            2,
            program.statements.len(),
            "Parser errors: {:?}",
            parser.errors
        );

        assert_eq!(
            vec!(
                Statement::Expression(Expression::Prefix(
                    PrefixOperator::Minus,
                    Box::new(Expression::Integer(5))
                )),
                Statement::Expression(Expression::Prefix(
                    PrefixOperator::Bang,
                    Box::new(Expression::Integer(15))
                )),
            ),
            program.statements,
        );
    }

    #[test]
    fn infix_expression() {
        let tests = vec![
            ("5 + 5;", 5, InfixOperator::Plus, 5),
            ("5 - 5;", 5, InfixOperator::Minus, 5),
            ("5 * 5;", 5, InfixOperator::Asterisk, 5),
            ("5 / 5;", 5, InfixOperator::Slash, 5),
            ("5 > 5;", 5, InfixOperator::Gt, 5),
            ("5 < 5;", 5, InfixOperator::Lt, 5),
            ("5 == 5;", 5, InfixOperator::Eq, 5),
            ("5 != 5;", 5, InfixOperator::NotEq, 5),
        ];

        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Infix(
                    operator,
                    Box::new(Expression::Integer(left)),
                    Box::new(Expression::Integer(right))
                ))]
            );
        }
    }

    #[test]
    fn boolean_expression() {
        let tests = vec![("true;", true), ("false;", false)];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            check_parser_errors(&parser);

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Boolean(expected)),]
            );
        }
    }

    #[test]
    fn operator_precedence() {
        test_parsing(vec![
            ("-a * b", "((-a) * b);"),
            ("!-a", "(!(-a));"),
            ("a + b + c", "((a + b) + c);"),
            ("a + b - c", "((a + b) - c);"),
            ("a * b * c", "((a * b) * c);"),
            ("a * b / c", "((a * b) / c);"),
            ("a + b / c", "(a + (b / c));"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -5 * 5", "(3 + 4);((-5) * 5);"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
        ]);
    }

    fn test_parsing(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.to_string(), expected);
        }
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();
        if errors.len() > 0 {
            panic!("got parser errors: {:?}", errors);
        }
    }
}
