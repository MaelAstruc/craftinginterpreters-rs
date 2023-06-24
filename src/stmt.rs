use crate::make_expr; // from mod utils
use crate::expr::Expr;

make_expr!(Expression<T: Expr>, expression: T);

make_expr!(Print<T: Expr>, expression: T);
