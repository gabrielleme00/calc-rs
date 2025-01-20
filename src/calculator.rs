/// Unary operation.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum UnOperation {
    Plus,
    Minus,
}

/// Binary operation.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum BiOperation {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    And,
    Or,
    Xor,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Token {
    Operand(i32),
    BiOperator(BiOperation),
    UnOperator(UnOperation),
    Bracket(char),
}

#[derive(Debug)]
pub enum CalcError {
    BadToken(char),
    MismatchedParens,
    MissingOperand,
    UnusedTokens,
}

impl std::fmt::Display for CalcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BadToken(c) => write!(f, "Bad token encountered: '{}'", c),
            Self::MismatchedParens => write!(f, "Mismatched parentheses"),
            Self::MissingOperand => write!(f, "Missing operand"),
            Self::UnusedTokens => write!(f, "There are unused tokens"),
        }
    }
}

/// Returns a digit based on the ASCII value of a char.
fn as_digit(c: char) -> i32 {
    c as i32 - 48
}

/// Tokenizes a string of an infix mathematical expression.
fn parse<T: AsRef<str>>(expr: T) -> Result<Vec<Token>, CalcError> {
    use Token::*;
    use BiOperation::*;
    use UnOperation::*;
    use CalcError::*;

    let expr = expr.as_ref();
    let chars = expr.chars();
    let mut tokens: Vec<Token> = Vec::new();
    let mut parens: Vec<char> = Vec::new();

    for c in chars {
        match c {
            '0'..='9' => match tokens.last_mut() {
                Some(Operand(n)) => *n = *n * 10 + as_digit(c),
                _ => tokens.push(Operand(as_digit(c))),
            },
            '(' => {
                tokens.push(Bracket(c));
                parens.push(c);
            }
            ')' => {
                tokens.push(Bracket(c));
                if let Some(p) = parens.pop() {
                    if p != '(' {
                        return Err(MismatchedParens);
                    }
                } else {
                    return Err(MismatchedParens);
                }
            }
            '+' => match tokens.last() {
                Some(Operand(_)) | Some(Bracket(')')) => {
                    tokens.push(BiOperator(Add))
                }
                _ => tokens.push(UnOperator(Plus)),
            },
            '-' => match tokens.last() {
                Some(Operand(_)) | Some(Bracket(')')) => {
                    tokens.push(BiOperator(Sub))
                }
                _ => tokens.push(UnOperator(Minus)),
            },
            '*' => {
                if tokens.ends_with(&[BiOperator(Mul)]) {
                    tokens.pop();
                    tokens.push(BiOperator(Pow));
                } else {
                    tokens.push(BiOperator(Mul))
                }
            },
            '/' => tokens.push(BiOperator(Div)),
            '%' => tokens.push(BiOperator(Mod)),
            '&' => tokens.push(BiOperator(And)),
            '|' => tokens.push(BiOperator(Or)),
            '^' => tokens.push(BiOperator(Xor)),
            ' ' | '\n' | '\r' => {}
            _ => return Err(BadToken(c)),
        }
    }

    if parens.len() > 0 {
        return Err(MismatchedParens);
    }

    Ok(tokens)
}

/// Transforms infix tokens into postfix (RPN).
fn postfix(mut tokens: Vec<Token>) -> Vec<Token> {
    use Token::*;

    let mut queue: Vec<Token> = Vec::new();
    let mut stack: Vec<Token> = Vec::new();

    tokens.reverse();

    while let Some(token) = tokens.pop() {
        match token {
            Operand(_) | UnOperator(_) => queue.push(token),
            BiOperator(_) => {
                while !stack.is_empty() && stack[stack.len() - 1] >= token {
                    queue.push(stack.pop().unwrap());
                }
                stack.push(token);
            }
            Bracket('(') => stack.push(token),
            Bracket(')') => {
                while !stack.is_empty() && stack[stack.len() - 1] != Bracket('(') {
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
fn evaluate(mut tokens: Vec<Token>) -> Result<f32, CalcError> {
    use Token::*;
    use BiOperation::*;
    use UnOperation::*;
    use CalcError::*;

    let mut stack: Vec<f32> = Vec::new();
    let mut unary: Vec<UnOperation> = Vec::new();

    tokens.reverse();

    while let Some(token) = tokens.pop() {
        match token {
            UnOperator(operator) => unary.push(operator),
            Operand(mut n) => {
                while let Some(operator) = unary.pop() {
                    n = match operator {
                        Plus => n,
                        Minus => -n,
                    };
                }
                stack.push(n as f32)
            }
            BiOperator(operator) => {
                let (right, left): (f32, f32) = match (stack.pop(), stack.pop()) {
                    (Some(n1), Some(n2)) => (n1, n2),
                    _ => return Err(MissingOperand),
                };
                let result = match operator {
                    Add => left + right,
                    Sub => left - right,
                    Mul => left * right,
                    Div => left / right,
                    Mod => left % right,
                    Pow => left.powf(right),
                    And => ((left as i32) & (right as i32)) as f32,
                    Or => ((left as i32) | (right as i32)) as f32,
                    Xor => ((left as i32) ^ (right as i32)) as f32,
                };
                stack.push(result)
            }
            _ => {}
        }
    }

    if stack.len() > 1 {
        return Err(UnusedTokens);
    }

    Ok(stack.pop().unwrap())
}

pub fn calculate<T: AsRef<str>>(expr: T) -> Result<f32, CalcError> {
    match parse(expr) {
        Ok(tokens) => evaluate(postfix(tokens)),
        Err(error) => Err(error),
    }
}
