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

impl std::fmt::Display for BiOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::Pow => "**",
            Self::And => "&",
            Self::Or => "|",
            Self::Xor => "^",
        })
    }
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

/// Returns the priority of a given Token.
fn precedence(op: &Token) -> i32 {
    use Token::*;
    use BiOperation::*;

    match op {
        BiOperator(Add) | BiOperator(Sub) => 1,
        BiOperator(Mul) | BiOperator(Div) => 2,
        _ => 0,
    }
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
                while let Some(last_token) = stack.last() {
                    if precedence(last_token) >= precedence(&token) {
                        queue.push(stack.pop().unwrap());
                    } else {
                        break;
                    }
                }
                stack.push(token);
            }
            Bracket('(') => stack.push(token),
            Bracket(')') => {
                while let Some(op) = stack.pop() {
                    if op == Bracket('(') {
                        break;
                    }
                    queue.push(op);
                }
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

#[cfg(test)]
mod tests {
    use std::f32::INFINITY;

    use super::*;

    #[test]
    fn test_basic_operations() {
        assert_eq!(calculate("100 + 200").unwrap(), 300.0);
        assert_eq!(calculate("20 - 1").unwrap(), 19.0);
        assert_eq!(calculate("3 * 40").unwrap(), 120.0);
        assert_eq!(calculate("8 / 2").unwrap(), 4.0);
    }

    #[test]
    fn test_complex_expressions() {
        let result = calculate("3 + (2 * (5 - (4 / 2)))").unwrap();
        assert_eq!(result, 9.0);
        
        let result = calculate("(10 + (30 / (2*3)) - 20)").unwrap();
        assert_eq!(result, -5.0);
    }

    #[test]
    fn test_order_of_operations() {
        assert_eq!(calculate("1 + 2 * 3").unwrap(), 7.0);
        assert_eq!(calculate("(1 + 2) * 3").unwrap(), 9.0);
        assert_eq!(calculate("8 - 4 / 2").unwrap(), 6.0);
    }

    #[test]
    fn test_unary_operations() {
        assert_eq!(calculate("-5").unwrap(), -5.0);
        assert_eq!(calculate("--5").unwrap(), 5.0);
        assert_eq!(calculate("---5").unwrap(), -5.0);
        assert_eq!(calculate("-3 + 5").unwrap(), 2.0);
    }

    #[test]
    fn test_bitwise_operations() {
        assert_eq!(calculate("1 & 3").unwrap(), 1.0); // AND
        assert_eq!(calculate("1 | 2").unwrap(), 3.0); // OR
        assert_eq!(calculate("3 ^ 4").unwrap(), 7.0); // XOR
    }

    #[test]
    fn test_error_handling() {
        assert!(calculate("1 +").is_err());
        assert!(calculate("* 3").is_err());
        assert!(calculate("(1 + 2").is_err());
        assert_eq!(calculate("3 / 0").unwrap(), INFINITY);
    }
}

