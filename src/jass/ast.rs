
// AST

pub type Ident = String;

#[derive(PartialEq, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    EQ,
    NE,
    GT,
    GE,
    LT,
    LE,
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    StringLiteral(String),
    IntLiteral(i32),
    RealLiteral(f32),
    VarAccess(Ident),
    ArrayAccess(Ident, Box<Expr>),
    CodeRef(Ident),
    Null,
    Call(Ident, Vec<Expr>),
    BinaryOp(Box<Expr>, BinaryOp, Box<Expr>),
    UnaryMinus(Box<Expr>)
}

// var_type [array] var_name = [expr]
#[derive(PartialEq, Debug)]
pub struct VariableDefinition {
    pub var_name: Ident,
    pub is_array: bool,
    pub var_type: Ident,
    pub assignment: Option<Expr>
}

#[derive(PartialEq, Debug)]
pub enum CodeStatement {
    // set <var_name> = <expr>
    VariableSet {
        var_name: Ident,
        expr: Expr
    },
    // call <func_name>(<args>)
    FunctionCall {
        func_name: Ident,
        args: Vec<Expr>
    },
    // loop
    //   <body>
    // endloop
    Loop {
        body: CodeBlock
    },
    // if <condition> then
    //   <body_then>
    // [else]
    //   [body_else]
    // endif
    If {
        condition: Expr,
        body_then: CodeBlock,
        body_else: Option<CodeBlock>
    },
    // return <expr>
    Return {
        expr: Expr
    },
    // exitwhen <expr>
    ExitWhen {
        expr: Expr
    }
}

#[derive(PartialEq, Debug)]
pub struct CodeBlock {
    statements: Vec<CodeStatement>
}