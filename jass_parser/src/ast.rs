
// AST

use crate::lexer::Spanned;
use crate::lexer::spanned;

type Sp<T> = Spanned<T>;

pub type Ident<'input> = &'input str;
pub type SpannedIdent<'input> = Sp<Ident<'input>>;
pub type SpannedExpr<'input> = Sp<Expr<'input>>;

#[derive(PartialEq, Debug)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Mul,
    Div,
    EQ,
    NE,
    GT,
    GE,
    LT,
    LE,
    Not,
    And,
    Or
}

#[derive(PartialEq, Debug)]
pub enum UnaryOperator {
    Not,
    Minus,
    Plus,
}

#[derive(PartialEq, Debug)]
pub struct VariableRef<'input> {
    pub ident: SpannedIdent<'input>
}

#[derive(PartialEq, Debug)]
pub struct ArrayRef<'input> {
    pub ident: SpannedIdent<'input>,
    pub index: Box<SpannedExpr<'input>>,
}

#[derive(PartialEq, Debug)]
pub struct CodeRef<'input> {
    pub ident: SpannedIdent<'input>
}

#[derive(PartialEq, Debug)]
pub struct FunctionCall<'input> {
    pub ident: SpannedIdent<'input>,
    pub args: Vec<SpannedExpr<'input>>
}

#[derive(PartialEq, Debug)]
pub struct BinaryOperation<'input> {
    pub lhs: Box<SpannedExpr<'input>>,
    pub rhs: Box<SpannedExpr<'input>>,
    pub operator: BinaryOperator,
}

#[derive(PartialEq, Debug)]
pub struct UnaryOperation<'input> {
    pub operator: UnaryOperator,
    pub arg: Box<SpannedExpr<'input>>,
}

#[derive(PartialEq, Debug)]
pub enum Literal<'input> {
    Str(&'input str),
    Int(u32),
    Real(f32),
    Bool(bool),
    Null
}

#[derive(PartialEq, Debug)]
pub enum Expr<'input> {
    Literal(Literal<'input>),
    VarAccess(VariableRef<'input>),
    ArrayAccess(ArrayRef<'input>),
    CodeRef(CodeRef<'input>),
    Call(FunctionCall<'input>),
    BinaryOperation(BinaryOperation<'input>),
    UnaryOperation(UnaryOperation<'input>)
}

#[derive(PartialEq, Debug)]
pub enum NativeType {
    Int,
    Real,
    Str,
    Bool,
    Handle,
    Code,
}

#[derive(PartialEq, Debug)]
pub enum Type<'input> {
    Native(NativeType),
    Defined(SpannedIdent<'input>)
}

#[derive(PartialEq, Debug)]
pub struct FunctionArg<'input> {
    pub arg_name: SpannedIdent<'input>,
    pub arg_type: Sp<Type<'input>>
}

#[derive(PartialEq, Debug)]
pub enum FunctionArgs<'input> {
    Nothing,
    List(Vec<FunctionArg<'input>>),
}

#[derive(PartialEq, Debug)]
pub enum FunctionReturns<'input> {
    Nothing,
    Type(Type<'input>)
}

#[derive(PartialEq, Debug)]
pub struct FunctionSignature<'input> {
    pub name: SpannedIdent<'input>,
    pub args: FunctionArgs<'input>,
    pub returns: FunctionReturns<'input>
}

#[derive(PartialEq, Debug)]
pub struct VariableDeclaration<'input> {
    pub is_array: bool,
    pub var_name: SpannedIdent<'input>,
    pub var_type: Type<'input>,
    pub var_assignment: Option<SpannedExpr<'input>>,
}

#[derive(PartialEq, Debug)]
pub struct LocalDeclaration<'input> {
    pub inner: VariableDeclaration<'input>
}

#[derive(PartialEq, Debug)]
pub struct GlobalDeclaration<'input> {
    pub is_constant: bool,
    pub inner: VariableDeclaration<'input>
}

#[derive(PartialEq, Debug)]
pub struct FunctionDefinition<'input> {
    pub is_constant: bool,
    pub signature: FunctionSignature<'input>,
    pub locals: Vec<LocalDeclaration<'input>>,
    pub body: CodeBlock<'input>
}

#[derive(PartialEq, Debug)]
pub struct NativeDefinition<'input> {
    pub is_constant: bool,
    pub signature: FunctionSignature<'input>
}

#[derive(PartialEq, Debug)]
pub struct TypeDefinition<'input> {
    pub lhs: Type<'input>,
    pub rhs: Type<'input>
}

#[derive(PartialEq, Debug)]
pub struct Conditional<'input> {
    pub condition: SpannedExpr<'input>,
    pub body: CodeBlock<'input>
}

#[derive(PartialEq, Debug)]
pub enum CodeStatement<'input> {
    VariableAssignment {
        var: VariableRef<'input>,
        expr: SpannedExpr<'input>
    },
    ArrayAssignment {
        array: ArrayRef<'input>,
        expr: SpannedExpr<'input>
    },
    FunctionCall {
        func_name: SpannedIdent<'input>,
        args: Vec<SpannedExpr<'input>>
    },
    Loop {
        body: CodeBlock<'input>
    },
    If {
        conditionals: Vec<Conditional<'input>>,
        body_else: Option<CodeBlock<'input>>
    },
    Return {
        expr: Option<SpannedExpr<'input>>
    },
    ExitWhen {
        expr: SpannedExpr<'input>
    }
}

#[derive(PartialEq, Debug)]
pub struct CodeBlock<'input> {
    pub statements: Vec<Sp<CodeStatement<'input>>>
}

#[derive(PartialEq, Debug)]
pub struct GlobalBlock<'input> {
    pub declarations: Vec<GlobalDeclaration<'input>>
}

#[derive(PartialEq, Debug)]
pub enum ProgramElement<'input> {
    NativeFunction(NativeDefinition<'input>),
    UserFunction(FunctionDefinition<'input>),
    Globals(GlobalBlock<'input>),
    Type(TypeDefinition<'input>),
}

#[derive(PartialEq, Debug)]
pub struct Program<'input> {
    pub elements: Vec<Sp<ProgramElement<'input>>>
}