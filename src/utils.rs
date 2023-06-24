#[macro_export]
macro_rules! make_expr {
    (
        $name:ident
        $(<$($generics:tt: $trait:ident),*>)?, 
        $($element: ident: $ty: ty), *
    ) => {
        pub struct $name $(<$($generics:$trait),*>)? {
            $(pub $element: $ty), *
        }
    }
}
