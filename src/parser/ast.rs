#[derive(Debug, PartialEq, Clone)]
pub enum ProgramFlow<'a> {
    Content(&'a str),
    Statement(Statement<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    Let {
        identifier: Identifier<'a>,
        expression: Expression<'a>,
    },
    For {
        identifier: Identifier<'a>,
        iterable: Expression<'a>,
        body: Scope<'a>,
    },
    If {
        condition: Expression<'a>,
        else_if_blocks: Vec<(Expression<'a>, Scope<'a>)>,
        then_block: Scope<'a>,
        else_block: Option<Scope<'a>>,
    },
    Return(Option<Expression<'a>>),
    Break,
    Continue,
    Display(Expression<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Scope<'a>(pub Vec<ProgramFlow<'a>>);

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    Identifier(Identifier<'a>),
    Literal(Literal<'a>),
    Binary(BinaryExpression<'a>),
    Unary(UnaryExpression<'a>),
    FunctionCall(FunctionCall<'a>),
    Index(IndexExpression<'a>),
    MemberAccess(MemberAccessExpression<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier<'a> {
    pub name: &'a str,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'a> {
    String(&'a str),
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpression<'a> {
    pub operator: BinaryOperator,
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Remainder,
    Exponent,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterOrEqual,
    LessOrEqual,
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpression<'a> {
    pub operator: UnaryOperator,
    pub expression: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Not,
    Negative,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall<'a> {
    pub function_name: Identifier<'a>,
    pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub index: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberAccessExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub member: Identifier<'a>,
}
