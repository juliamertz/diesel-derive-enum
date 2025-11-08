#![recursion_limit = "1024"]

extern crate proc_macro;

use heck::{ToKebabCase, ToLowerCamelCase, ToShoutySnakeCase, ToSnakeCase, ToUpperCamelCase};
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, Attribute, Data, DeriveInput, Fields, LitByteStr,
    LitStr, Meta, Result, Variant,
};

/// Implement the traits necessary for inserting the enum directly into a database as TEXT
///
/// # Attributes
///
/// ## Enum-level attributes
///
/// * `#[db_enum(value_style = "snake_case")]` specifies a renaming style from each of
///   the rust enum variants to each of the database string values. Either `camelCase`,
///   `kebab-case`, `PascalCase`, `SCREAMING_SNAKE_CASE`, `snake_case`,
///   `verbatim`. If omitted, uses `snake_case`.
///
/// ## Variant attributes
///
/// * `#[db_enum(rename = "renamed-variant")]` specifies the db string value for a specific variant.
#[proc_macro_derive(DbEnum, attributes(db_enum))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);

    // Gather and validate enum-level attributes
    let attrs = match gather_db_enum_attrs(&input.attrs) {
        Ok(ok) => ok,
        Err(e) => return e.to_compile_error().into(),
    };

    let case_style = CaseStyle::from_string(
        &attrs
            .value_style
            .unwrap_or_else(|| "snake_case".to_string()),
    );

    if let Data::Enum(syn::DataEnum {
        variants: data_variants,
        ..
    }) = input.data
    {
        generate_derive_enum_impls(case_style, &input.ident, &data_variants)
    } else {
        syn::Error::new(
            Span::call_site(),
            "derive(DbEnum) can only be applied to enums",
        )
        .to_compile_error()
        .into()
    }
}

/// Container for all enum-level attributes for DbEnum
#[derive(Debug, Default)]
struct DbEnumAttrs {
    value_style: Option<String>,
}

/// Defines the casing for the database representation. Follows serde naming convention.
#[derive(Copy, Clone, Debug, PartialEq)]
enum CaseStyle {
    Camel,
    Kebab,
    Pascal,
    Upper,
    ScreamingSnake,
    Snake,
    Verbatim,
}

impl CaseStyle {
    fn from_string(name: &str) -> Self {
        match name {
            "camelCase" => CaseStyle::Camel,
            "kebab-case" => CaseStyle::Kebab,
            "PascalCase" => CaseStyle::Pascal,
            "SCREAMING_SNAKE_CASE" => CaseStyle::ScreamingSnake,
            "UPPERCASE" => CaseStyle::Upper,
            "snake_case" => CaseStyle::Snake,
            "verbatim" | "verbatimcase" => CaseStyle::Verbatim,
            s => panic!("unsupported casing: `{}`", s),
        }
    }
}

/// Gather and validate all db_enum attributes from a list of attributes
fn gather_db_enum_attrs(attrs: &[Attribute]) -> Result<DbEnumAttrs> {
    let mut result = DbEnumAttrs::default();

    for attr in attrs.iter() {
        if attr.path().is_ident("db_enum") {
            let Meta::List(nested) = &attr.meta else {
                continue;
            };

            // Process all the nested meta items in this db_enum attribute
            nested.parse_nested_meta(|meta| {
                let attr_name = meta
                    .path
                    .get_ident()
                    .ok_or_else(|| meta.error("expected ident"))?
                    .to_string();
                match attr_name.as_str() {
                    "value_style" => {
                        if let Ok(value) = meta.value()?.parse::<LitStr>() {
                            result.value_style = Some(value.value());
                        }
                    }
                    other => {
                        return Err(meta.error(format!("Unknown attribute: '{other}'")));
                    }
                }
                Ok(())
            })?;
        }
    }
    Ok(result)
}

/// Gets a variant-level attribute value, with validation for attribute names
fn get_variant_db_enum_attr_value(attrs: &[Attribute]) -> Result<Option<String>> {
    for attr in attrs.iter() {
        if attr.path().is_ident("db_enum") {
            let Meta::List(nested) = &attr.meta else {
                continue;
            };

            let mut result: Option<String> = None;

            nested.parse_nested_meta(|meta| {
                let attr_name = meta
                    .path
                    .get_ident()
                    .ok_or_else(|| meta.error("expected ident"))?;
                if attr_name == "rename" {
                    if let Ok(value) = meta.value()?.parse::<LitStr>() {
                        result = Some(value.value());
                        return Ok(());
                    } else {
                        return Err(meta.error("attribute 'rename' has no value"));
                    }
                } else {
                    return Err(meta.error(format!("Unhandled attribute: '{attr_name}'")));
                }
            })?;

            if result.is_some() {
                return Ok(result);
            }
        }
    }
    Ok(None)
}

fn generate_derive_enum_impls(
    case_style: CaseStyle,
    enum_ty: &Ident,
    variants: &Punctuated<Variant, syn::token::Comma>,
) -> TokenStream {
    let modname = Ident::new(&format!("db_enum_impl_{}", enum_ty), Span::call_site());
    let variant_ids: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|variant| {
            if let Fields::Unit = variant.fields {
                let id = &variant.ident;
                quote! {
                    #enum_ty::#id
                }
            } else {
                panic!("Variants must be fieldless")
            }
        })
        .collect();

    let variants_db: Vec<String> = match variants
        .iter()
        .map(|variant| {
            let rename_result = get_variant_db_enum_attr_value(&variant.attrs)?;
            match rename_result {
                Some(rename) => Ok(rename),
                None => Ok(stylize_value(&variant.ident.to_string(), case_style)),
            }
        })
        .collect::<Result<Vec<_>>>()
    {
        Ok(ok) => ok,
        Err(e) => return e.to_compile_error().into(),
    };
    let variants_db_bytes: Vec<LitByteStr> = variants_db
        .iter()
        .map(|variant_str| LitByteStr::new(variant_str.as_bytes(), Span::call_site()))
        .collect();

    let common = generate_common(enum_ty, &variant_ids, &variants_db, &variants_db_bytes);
    let common_impls = generate_common_impls(enum_ty);

    let pg_impl = if cfg!(feature = "postgres") {
        Some(generate_postgres_impl(enum_ty))
    } else {
        None
    };

    let mysql_impl = if cfg!(feature = "mysql") {
        Some(generate_mysql_impl(enum_ty))
    } else {
        None
    };

    let sqlite_impl = if cfg!(feature = "sqlite") {
        Some(generate_sqlite_impl(enum_ty))
    } else {
        None
    };

    let imports = quote! {
        use super::*;
        use diesel::{
            backend::{self, Backend},
            deserialize::{self, FromSql},
            expression::AsExpression,
            internal::derives::as_expression::Bound,
            serialize::{self, IsNull, Output, ToSql},
            sql_types::*,
            Queryable,
        };
        use std::io::Write;
    };

    let quoted = quote! {
        #[allow(non_snake_case)]
        mod #modname {
            #imports

            #common
            #common_impls
            #pg_impl
            #mysql_impl
            #sqlite_impl
        }
    };

    quoted.into()
}

fn stylize_value(value: &str, style: CaseStyle) -> String {
    match style {
        CaseStyle::Camel => value.to_lower_camel_case(),
        CaseStyle::Kebab => value.to_kebab_case(),
        CaseStyle::Pascal => value.to_upper_camel_case(),
        CaseStyle::Upper => value.to_uppercase(),
        CaseStyle::ScreamingSnake => value.to_shouty_snake_case(),
        CaseStyle::Snake => value.to_snake_case(),
        CaseStyle::Verbatim => value.to_string(),
    }
}

fn generate_common(
    enum_ty: &Ident,
    variants_rs: &[proc_macro2::TokenStream],
    variants_db: &[String],
    variants_db_bytes: &[LitByteStr],
) -> proc_macro2::TokenStream {
    quote! {
        fn db_str_representation(e: &#enum_ty) -> &'static str {
            match *e {
                #(#variants_rs => #variants_db,)*
            }
        }

        fn from_db_binary_representation(bytes: &[u8]) -> deserialize::Result<#enum_ty> {
            match bytes {
                #(#variants_db_bytes => Ok(#variants_rs),)*
                v => Err(format!("Unrecognized enum variant: '{}'",
                    String::from_utf8_lossy(v)).into()),
            }
        }
    }
}

fn generate_common_impls(enum_ty: &Ident) -> proc_macro2::TokenStream {
    quote! {
        impl AsExpression<Text> for #enum_ty {
            type Expression = Bound<Text, Self>;

            fn as_expression(self) -> Self::Expression {
                Bound::new(self)
            }
        }

        impl AsExpression<Nullable<Text>> for #enum_ty {
            type Expression = Bound<Nullable<Text>, Self>;

            fn as_expression(self) -> Self::Expression {
                Bound::new(self)
            }
        }

        impl<'a> AsExpression<Text> for &'a #enum_ty {
            type Expression = Bound<Text, Self>;

            fn as_expression(self) -> Self::Expression {
                Bound::new(self)
            }
        }

        impl<'a> AsExpression<Nullable<Text>> for &'a #enum_ty {
            type Expression = Bound<Nullable<Text>, Self>;

            fn as_expression(self) -> Self::Expression {
                Bound::new(self)
            }
        }

        impl<'a, 'b> AsExpression<Text> for &'a &'b #enum_ty {
            type Expression = Bound<Text, Self>;

            fn as_expression(self) -> Self::Expression {
                Bound::new(self)
            }
        }

        impl<'a, 'b> AsExpression<Nullable<Text>> for &'a &'b #enum_ty {
            type Expression = Bound<Nullable<Text>, Self>;

            fn as_expression(self) -> Self::Expression {
                Bound::new(self)
            }
        }

        impl<DB> ToSql<Nullable<Text>, DB> for #enum_ty
        where
            DB: Backend,
            Self: ToSql<Text, DB>,
        {
            fn to_sql<'b>(&'b self, out: &mut Output<'b, '_, DB>) -> serialize::Result {
                ToSql::<Text, DB>::to_sql(self, out)
            }
        }
    }
}

fn generate_postgres_impl(enum_ty: &Ident) -> proc_macro2::TokenStream {
    quote! {
        mod pg_impl {
            use super::*;
            use diesel::pg::{Pg, PgValue};

            impl FromSql<Text, Pg> for #enum_ty {
                fn from_sql(raw: PgValue) -> deserialize::Result<Self> {
                    from_db_binary_representation(raw.as_bytes())
                }
            }

            impl ToSql<Text, Pg> for #enum_ty {
                fn to_sql<'b>(&'b self, out: &mut Output<'b, '_, Pg>) -> serialize::Result {
                    out.write_all(db_str_representation(self).as_bytes())?;
                    Ok(IsNull::No)
                }
            }

            impl Queryable<Text, Pg> for #enum_ty {
                type Row = Self;

                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        }
    }
}

fn generate_mysql_impl(enum_ty: &Ident) -> proc_macro2::TokenStream {
    quote! {
        mod mysql_impl {
            use super::*;
            use diesel::mysql::{Mysql, MysqlValue};

            impl FromSql<Text, Mysql> for #enum_ty {
                fn from_sql(raw: MysqlValue) -> deserialize::Result<Self> {
                    from_db_binary_representation(raw.as_bytes())
                }
            }

            impl ToSql<Text, Mysql> for #enum_ty {
                fn to_sql<'b>(&'b self, out: &mut Output<'b, '_, Mysql>) -> serialize::Result {
                    out.write_all(db_str_representation(self).as_bytes())?;
                    Ok(IsNull::No)
                }
            }

            impl Queryable<Text, Mysql> for #enum_ty {
                type Row = Self;

                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        }
    }
}

fn generate_sqlite_impl(enum_ty: &Ident) -> proc_macro2::TokenStream {
    quote! {
        mod sqlite_impl {
            use super::*;
            use diesel::sql_types;
            use diesel::sqlite::Sqlite;

            impl FromSql<Text, Sqlite> for #enum_ty {
                fn from_sql(value: backend::RawValue<Sqlite>) -> deserialize::Result<Self> {
                    let bytes = <Vec<u8> as FromSql<sql_types::Binary, Sqlite>>::from_sql(value)?;
                    from_db_binary_representation(bytes.as_slice())
                }
            }

            impl ToSql<Text, Sqlite> for #enum_ty {
                fn to_sql<'b>(&'b self, out: &mut Output<'b, '_, Sqlite>) -> serialize::Result {
                    <str as ToSql<sql_types::Text, Sqlite>>::to_sql(db_str_representation(self), out)
                }
            }

            impl Queryable<Text, Sqlite> for #enum_ty {
                type Row = Self;

                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    #[test]
    fn test_db_enum_macro_validation() {
        // Test valid attribute
        let valid_attr: Attribute = parse_quote! {
            #[db_enum(value_style = "snake_case")]
        };

        let invalid_attr: Attribute = parse_quote! {
            #[db_enum(diesel_type = "MyType")]
        };

        // Test valid attribute
        let valid_result = gather_db_enum_attrs(&[valid_attr]);
        assert!(valid_result.is_ok(), "Valid attribute should be accepted");
        if let Ok(attrs) = valid_result {
            assert_eq!(attrs.value_style, Some("snake_case".to_string()));
        }

        // Test invalid attribute (should now be rejected)
        let invalid_result = gather_db_enum_attrs(&[invalid_attr]);
        assert!(
            invalid_result.is_err(),
            "Invalid attribute should be rejected"
        );
        if let Err(e) = invalid_result {
            let error_msg = e.to_string();
            assert!(
                error_msg.contains("diesel_type"),
                "Error message should mention the invalid attribute"
            );
        }
    }

    #[test]
    fn test_variant_attribute() {
        {
            let variant_attr: Attribute = parse_quote! {
                #[db_enum(rename = "custom_name")]
            };

            let result = get_variant_db_enum_attr_value(&[variant_attr.clone()]);
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), Some("custom_name".to_string()));
        }

        {
            let variant_attr_phony: Attribute = parse_quote! {
                #[db_enum(phony = "phony")]
            };

            let result = get_variant_db_enum_attr_value(&[variant_attr_phony.clone()]);
            assert!(result.is_err());
        }

        {
            let variant_attr_fake: Attribute = parse_quote! {
            #[db_enum(fake)]
            };

            let result = get_variant_db_enum_attr_value(&[variant_attr_fake.clone()]);
            assert!(result.is_err());
        }
    }
}
