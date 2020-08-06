use crate::ast::{Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    ExpectedAssign(Token),
    ExpectedIdentifierToken(Token),
}

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

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => unimplemented!(),
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

    fn expect_peek(&mut self, token: Token, expected: fn(Token) -> ParserError) -> Result<()> {
        if self.peek_token != token {
            return Err(expected(self.peek_token.clone()));
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Statement;
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
}
