#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum UnOperator {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum BiOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Token {
    Operand(i32),
    BiOp(BiOperator),
    UnOp(UnOperator),
    Bracket(char),
}

#[derive(Debug)]
pub enum Error {
    BadToken(char),
    MismatchedParens,
    MissingOperand,
    UnusedTokens,
}

pub fn calculate<T: AsRef<str>>(expr: T) -> Result<f32, Error> {
    match parse(expr) {
        Ok(tokens) => evaluate(postfix(tokens)),
        Err(error) => Err(error),
    }
}

/// Tokenizes a string of an infix mathematical expression.
fn parse<T: AsRef<str>>(expr: T) -> Result<Vec<Token>, Error> {
    let expr = expr.as_ref();
    let chars = expr.chars();
    let mut tokens: Vec<Token> = Vec::new();
    let mut parens: Vec<char> = Vec::new();
    for c in chars {
        match c {
            '0'..='9' => match tokens.last_mut() {
                Some(Token::Operand(n)) => *n = *n * 10 + as_digit(c),
                _ => tokens.push(Token::Operand(as_digit(c))),
            },
            '(' => {
                tokens.push(Token::Bracket(c));
                parens.push(c);
            }
            ')' => {
                tokens.push(Token::Bracket(c));
                if let Some(p) = parens.pop() {
                    if p != '(' {
                        return Err(Error::MismatchedParens);
                    }
                } else {
                    return Err(Error::MismatchedParens);
                }
            }
            '+' => match tokens.last() {
                Some(Token::Operand(_)) | Some(Token::Bracket(')')) => {
                    tokens.push(Token::BiOp(BiOperator::Add))
                }
                _ => tokens.push(Token::UnOp(UnOperator::Plus)),
            },
            '-' => match tokens.last() {
                Some(Token::Operand(_)) | Some(Token::Bracket(')')) => {
                    tokens.push(Token::BiOp(BiOperator::Sub))
                }
                _ => tokens.push(Token::UnOp(UnOperator::Minus)),
            },
            '*' => tokens.push(Token::BiOp(BiOperator::Mul)),
            '/' => tokens.push(Token::BiOp(BiOperator::Div)),
            '%' => tokens.push(Token::BiOp(BiOperator::Mod)),
            '^' => tokens.push(Token::BiOp(BiOperator::Pow)),
            ' ' | '\n' | '\r' => {}
            _ => return Err(Error::BadToken(c)),
        }
    }

    if parens.len() > 0 {
        return Err(Error::MismatchedParens);
    }

    Ok(tokens)
}

/// Transforms infix tokens into postfix (RPN).
fn postfix(mut tokens: Vec<Token>) -> Vec<Token> {
    let mut queue: Vec<Token> = Vec::new();
    let mut stack: Vec<Token> = Vec::new();

    tokens.reverse();

    while let Some(token) = tokens.pop() {
        match token {
            Token::Operand(_) | Token::UnOp(_) => queue.push(token),
            Token::BiOp(_) => {
                while !stack.is_empty() && stack[stack.len() - 1] >= token {
                    queue.push(stack.pop().unwrap());
                }
                stack.push(token);
            }
            Token::Bracket('(') => stack.push(token),
            Token::Bracket(')') => {
                while !stack.is_empty() && stack[stack.len() - 1] != Token::Bracket('(') {
                    queue.push(stack.pop().unwrap());
                }
                stack.pop();
            }
            _ => {}
        }
    }

    while stack.len() > 0 {
        queue.push(stack.pop().unwrap());
    }

    queue
}

/// Evaluates an RPL expression.
fn evaluate(mut tokens: Vec<Token>) -> Result<f32, Error> {
    let mut stack: Vec<f32> = Vec::new();
    let mut unary: Vec<UnOperator> = Vec::new();

    tokens.reverse();

    while let Some(token) = tokens.pop() {
        match token {
            Token::UnOp(operator) => unary.push(operator),
            Token::Operand(mut n) => {
                while let Some(operator) = unary.pop() {
                    n = match operator {
                        UnOperator::Plus => n,
                        UnOperator::Minus => -n,
                    };
                }
                stack.push(n as f32)
            }
            Token::BiOp(operator) => {
                let (right, left): (f32, f32) = match (stack.pop(), stack.pop()) {
                    (Some(n1), Some(n2)) => (n1, n2),
                    _ => return Err(Error::MissingOperand),
                };
                match operator {
                    BiOperator::Add => stack.push(left + right),
                    BiOperator::Sub => stack.push(left - right),
                    BiOperator::Mul => stack.push(left * right),
                    BiOperator::Div => stack.push(left / right),
                    BiOperator::Mod => stack.push(left % right),
                    BiOperator::Pow => stack.push(left.powf(right)),
                }
            }
            _ => {}
        }
    }

    if stack.len() > 1 {
        return Err(Error::UnusedTokens);
    }

    Ok(stack.pop().unwrap())
}

/// Returns a digit based on the ASCII value of a char.
fn as_digit(c: char) -> i32 {
    c as i32 - 48
}
