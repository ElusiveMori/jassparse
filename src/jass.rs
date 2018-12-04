mod ast;

#[allow(clippy::all, dead_code, unused_mut)]
pub mod syntax;
pub mod lexer;

#[cfg(test)]
mod tests {
    use crate::jass::syntax;
    use crate::jass::ast::*;

    macro_rules! matches(
        ($e:expr, $p:pat) => (
            match $e {
                $p => true,
                _ => false
            }
        )
    );

    fn evaluate_int_expr(expr: &Expr) -> i32 {
        match &expr {
            Expr::IntLiteral(num) => *num,
            Expr::BinaryOp(lhs, op, rhs) => {
                let lhsv = evaluate_int_expr(lhs);
                let rhsv = evaluate_int_expr(rhs);

                match op {
                    BinaryOp::Add => lhsv + rhsv,
                    BinaryOp::Sub => lhsv - rhsv,
                    BinaryOp::Mul => lhsv * rhsv,
                    BinaryOp::Div => lhsv / rhsv,
                    _ => panic!("not an integer arithmetic operator")
                }
            }
            _ => panic!("not an integer arithmetic expression")
        }
    }

    #[test]
    fn test_int_parsing() {
        assert!(syntax::IntLiteralParser::new().parse("0x01AB").is_ok());
        assert!(syntax::IntLiteralParser::new().parse("$01AB").is_ok());

        assert!(syntax::IntLiteralParser::new().parse("000001").is_ok());
        assert!(syntax::IntLiteralParser::new().parse("123012").is_ok());
        assert!(syntax::IntLiteralParser::new().parse("123_213").is_err());
        assert!(syntax::IntLiteralParser::new().parse("123012.1").is_err());
        assert!(syntax::IntLiteralParser::new().parse(".1").is_err());
    }

    #[test]
    fn test_real_parsing() {
        assert!(syntax::RealLiteralParser::new().parse("321").is_err());
        assert!(syntax::RealLiteralParser::new().parse("123.1").is_ok());
        assert!(syntax::RealLiteralParser::new().parse(".1").is_ok());
        assert!(syntax::RealLiteralParser::new().parse("1.").is_ok());
    }

    #[test]
    fn test_ident_str_parsing() {
        assert!(syntax::IdentStrParser::new().parse("321").is_err());
        assert!(syntax::IdentStrParser::new().parse("abc").is_ok());
        assert!(syntax::IdentStrParser::new().parse("abc_").is_ok());
        assert!(syntax::IdentStrParser::new().parse("_abc").is_ok());
        assert!(syntax::IdentStrParser::new().parse("abc_abc").is_ok());
        assert!(syntax::IdentStrParser::new().parse("3_abc").is_err());
        assert!(syntax::IdentStrParser::new().parse("$_abc").is_err());
        assert!(syntax::IdentStrParser::new().parse("abc$#").is_err());
    }

    #[test]
    fn test_int_expr_parsing() {
        let expr1 = syntax::ExprParser::new().parse("1 + 2 * 3 + 4 * 5 * 6 + 7 + 8 * 9").unwrap();
        let expr2 = syntax::ExprParser::new().parse("1 + (2 * 3) + (4 * 5 * 6) + 7 + (8 * 9)").unwrap();
        let expr3 = syntax::ExprParser::new().parse("1 * (2 + 3 * (4 + 5))").unwrap();

        assert!(expr1 == expr2);
        assert_eq!(evaluate_int_expr(&expr1), 206);
        assert_eq!(evaluate_int_expr(&expr2), 206);
        assert_eq!(evaluate_int_expr(&expr3), 29);
    }

    #[test]
    fn test_general_expr_parsin() {
        let expr = syntax::ExprParser::new().parse("some_array[1]").unwrap();
        
        assert!(matches!(expr, Expr::ArrayAccess(..)));
    }
}