use crate::ast::*;
use crate::parser::{Error, Parser};
use linked_hash_map::LinkedHashMap;

macro_rules! should_parse {
    ($func_name:ident, $should_parse:expr, $input:expr) => {
        #[test]
        fn $func_name() {
            let res = Parser::new().parse_query($input);
            let should_parse = $should_parse;
            if should_parse {
                res.expect("expected input to parse, but it failed");
            } else {
                assert!(res.is_err());
            }
        }
    };
}

macro_rules! should_fail_to_parse_with_error {
    ($func_name:ident, $expected_error:expr, $input:expr) => {
        #[test]
        fn $func_name() {
            let res = Parser::new().parse_query($input);
            match res {
                Ok(_) => panic!("expected parse error, but parsing succeeded"),
                Err(Error::Lalrpop(s)) => assert_eq!($expected_error.to_string(), s),
            }
        }
    };
}

macro_rules! validate_query_ast {
    ($func_name:ident, $input:expr, $ast:expr) => {
        #[test]
        fn $func_name() {
            let p = Parser::new();
            assert_eq!(p.parse_query($input).unwrap(), $ast)
        }
    };
}

macro_rules! validate_expression_ast {
    ($func_name:ident, $input:expr, $ast:expr) => {
        #[test]
        fn $func_name() {
            let p = Parser::new();
            assert_eq!(p.parse_expression($input).unwrap(), $ast)
        }
    };
}

// Select tests
should_parse!(select_star, true, "select *");
should_parse!(select_star_upper, true, "SELECT *");
should_parse!(select_mixed_case, true, "SeLeCt *");
should_parse!(select_a_star, true, "select a.*");
should_parse!(select_underscore_id, true, "select _id");
should_parse!(select_contains_underscore, true, "select a_b");
should_parse!(select_multiple, true, "select a,b,c");
should_parse!(select_multiple_combo, true, "select a,b,*");
should_parse!(select_multiple_star, true, "select *,*");
should_parse!(select_multiple_dot_star, true, "select a.*,b.*");
should_parse!(select_all_lower, true, "select all *");
should_parse!(select_all_upper, true, "select ALL *");
should_parse!(select_all_mixed_case, true, "select aLl *");
should_parse!(select_distinct_lower, true, "select distinct *");
should_parse!(select_distinct_upper, true, "select DISTINCT *");
should_parse!(select_distinct_mixed_case, true, "select DiSTinCt *");
should_parse!(select_value_lower, true, "SELECT value foo.*");
should_parse!(select_value_upper, true, "SELECT VALUE foo.*");
should_parse!(select_value_mixed_case, true, "SELECT vAlUe foo.*");
should_parse!(select_values_lower, true, "SELECT values foo.*, bar.*");
should_parse!(select_values_upper, true, "SELECT VALUES foo.*, bar.*");
should_parse!(select_values_mixed_case, true, "SELECT vAluES foo.*, bar.*");
should_parse!(select_alias_lower, true, "SELECT foo as f");
should_parse!(select_alias_upper, true, "SELECT foo AS f");
should_parse!(select_alias_mixed_case, true, "SELECT foo aS f");
should_parse!(select_alias_no_as, true, "SELECT foo f");
should_parse!(select_alias_compound_column, true, "SELECT a.b as a");
should_parse!(
    select_alias_multiple_combined,
    true,
    "SELECT a, b AS c, a.c"
);
should_parse!(select_long_compound, true, "SELECT a.b.c.d");
should_parse!(select_letter_number_ident, true, "SELECT a9");
should_parse!(select_delimited_ident_quotes, true, r#"SELECT "foo""#);
should_parse!(select_delimited_ident_backticks, true, "SELECT `foo`");
should_parse!(select_delimited_quote_empty, true, r#"SELECT """#);
should_parse!(select_delimited_backtick_empty, true, "SELECT ``");
should_parse!(
    select_delimited_escaped_quote,
    true,
    r#"SELECT "fo""o""""""#
);
should_parse!(
    select_delimited_escaped_backtick,
    true,
    "SELECT `f``oo`````"
);

should_parse!(use_stmt, false, "use foo");
should_parse!(select_compound_star, false, "SELECT a.b.c.*");
should_parse!(select_numerical_ident_prefix, false, "SELECT 9ae");
should_parse!(select_value_star, false, "SELECT VALUE *");
should_parse!(select_value_alias, false, "SELECT VALUE foo AS f");
should_parse!(select_dangling_alias, false, "SELECT a.b AS");
should_parse!(select_compound_alias, false, "SELECT a AS b.c");

should_parse!(
    select_delimited_extra_quote_outer,
    false,
    r#"SELECT ""foo"""#
);
should_parse!(
    select_delimited_extra_backtick_outer,
    false,
    "SELECT ``foo``"
);
should_parse!(
    select_delimited_escaped_quote_odd,
    false,
    r#"SELECT "f"oo"""#
);
should_parse!(
    select_delimited_escaped_backtick_odd,
    false,
    "SELECT `foo````"
);
should_parse!(
    select_delimited_backslash_escape,
    false,
    r#"SELECT "fo\"\"o""#
);
should_parse!(select_unescaped_quotes_in_ident, false, r#"SELECT fo""o"#);
should_parse!(select_unescaped_backticks_in_ident, false, "SELECT fo``o");

validate_query_ast!(
    ident,
    "SELECT foo",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier("foo".to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
validate_query_ast!(
    delimited_quote,
    r#"SELECT "foo""#,
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier("foo".to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
validate_query_ast!(
    delimited_backtick,
    "select `foo`",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier("foo".to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
validate_query_ast!(
    delimited_escaped_backtick,
    "SELECT `fo``o`````",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier("fo`o``".to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
validate_query_ast!(
    delimited_escaped_quote,
    r#"SELECT "fo""o""""""#,
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier(r#"fo"o"""#.to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
validate_query_ast!(
    backtick_delimiter_escaped_quote,
    r#"SELECT `fo""o`"#,
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier(r#"fo""o"#.to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
validate_query_ast!(
    quote_delimiter_escaped_backtick,
    r#"SELECT "fo``o""#,
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier("fo``o".to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);

// Set query tests
should_parse!(select_union_simple, true, "SELECT a UNION SELECT b");
should_parse!(
    select_union_multiple,
    true,
    "SELECT a UNION SELECT b UNION SELECT c"
);
should_parse!(
    select_union_all_multiple,
    true,
    "SELECT a UNION ALL SELECT b UNION ALL SELECT c"
);

validate_query_ast!(
    union_is_left_associative,
    "select a union select b union all select c",
    Query::Set(SetQuery {
        left: Box::new(Query::Set(SetQuery {
            left: Box::new(Query::Select(SelectQuery {
                select_clause: SelectClause {
                    set_quantifier: SetQuantifier::All,
                    body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                        expr: Expression::Identifier("a".to_string()),
                        alias: None
                    })])
                },
                from_clause: None,
                where_clause: None,
                group_by_clause: None,
                having_clause: None,
                order_by_clause: None,
                limit: None,
                offset: None,
            })),
            op: SetOperator::Union,
            right: Box::new(Query::Select(SelectQuery {
                select_clause: SelectClause {
                    set_quantifier: SetQuantifier::All,
                    body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                        expr: Expression::Identifier("b".to_string()),
                        alias: None
                    })])
                },
                from_clause: None,
                where_clause: None,
                group_by_clause: None,
                having_clause: None,
                order_by_clause: None,
                limit: None,
                offset: None,
            }))
        })),
        op: SetOperator::UnionAll,
        right: Box::new(Query::Select(SelectQuery {
            select_clause: SelectClause {
                set_quantifier: SetQuantifier::All,
                body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                    expr: Expression::Identifier("c".to_string()),
                    alias: None
                })])
            },
            from_clause: None,
            where_clause: None,
            group_by_clause: None,
            having_clause: None,
            order_by_clause: None,
            limit: None,
            offset: None,
        }))
    })
);

// Operator tests
should_parse!(unary_pos, true, "select +a");
should_parse!(unary_neg, true, "select -a");
should_parse!(unary_not, true, "select NOT a");
should_parse!(binary_add, true, "select a+b+c+d+e");
should_parse!(binary_sub, true, "select a-b-c-d-e");
should_parse!(binary_mul, true, "select a*b*c*d*e");
should_parse!(binary_mul_add, true, "select a*b+c*d+e");
should_parse!(binary_div_add, true, "select a/b+c/d+e");
should_parse!(binary_div_mul, true, "select a/b*c");
should_parse!(binary_num, true, "select 3+4-5/7*8");
should_parse!(binary_lt, true, "select a<b<c<d<e");
should_parse!(binary_lte, true, "select a<=b");
should_parse!(binary_gt, true, "select a>b");
should_parse!(binary_gte, true, "select a>=b");
should_parse!(binary_neq_1, true, "select a!=b");
should_parse!(binary_neq_2, true, "select a<>b");
should_parse!(binary_eq, true, "select a=b");
should_parse!(binary_string_concat, true, "select a || b");
should_parse!(binary_or, true, "select a OR b");
should_parse!(binary_and, true, "select a AND b");
should_parse!(binary_compare_and_add, true, "select b<a+c and b>d+e");
should_parse!(binary_compare_or_mul, true, "select b<a*c or b>d*e");
should_parse!(binary_lt_and_neq, true, "select a<b and c<>e");
should_parse!(between, true, "select a BETWEEN b AND c");
should_parse!(
    binary_between,
    true,
    "SELECT 1 between 0 and 3 and 2 between 1 and 3"
);
should_parse!(case, true, "select CASE WHEN a=b THEN a ELSE c END");
should_parse!(
    case_multiple_when_clauses,
    true,
    "select CASE WHEN a or b THEN a WHEN c=d THEN c ELSE e END"
);
should_parse!(
    case_multiple_exprs,
    true,
    "select CASE a WHEN a <> b THEN a WHEN c and d THEN c ELSE e END"
);
should_parse!(
    binary_case,
    true,
    "select CASE WHEN a=b THEN a ELSE c END + CASE WHEN c=d THEN c ELSE e END"
);
should_parse!(
    case_between,
    true,
    "select CASE when a BETWEEN b and c THEN a ELSE b END"
);
should_parse!(is_type, true, "select a IS STRING");
should_parse!(is_not_type, true, "select a-8 IS NOT DECIMAL(1)");
should_parse!(is_missing, true, "select a IS MISSING");
should_parse!(is_not_missing, true, "select (a+b) IS NOT MISSING");
should_parse!(like, true, "select col1 LIKE 'A%'");
should_parse!(
    not_like_multiple_spaces,
    true,
    "select col1 NOT   LIKE '[a-z][a-z]'"
);
should_parse!(like_escape, true, "select col1 LIKE '%a!% b' ESCAPE '!'");
should_parse!(
    not_like_escape,
    true,
    "select col1 NOT LIKE '%a!% b' ESCAPE '!'"
);
should_parse!(where_is, true, "select * where a IS NULL");
should_parse!(where_like, true, "select * where col1 LIKE 'A%'");

validate_expression_ast!(
    is_missing_ast,
    "a IS MISSING",
    Expression::Is(IsExpr {
        expr: Box::new(Expression::Identifier("a".to_string())),
        target_type: TypeOrMissing::Missing,
    })
);

should_parse!(between_invalid_binary_op, false, "select a BETWEEN b + c");
should_parse!(
    not_between_invalid_binary_op,
    false,
    "select a NOT BETWEEN b / c"
);
should_parse!(
    case_non_bool_conditions,
    false,
    "select case a when a+b then a else c-d"
);
validate_expression_ast!(
    binary_sub_unary_neg_paren_ast,
    "b-(-a)",
    Expression::Binary(BinaryExpr {
        left: Box::new(Expression::Identifier("b".to_string())),
        op: BinaryOp::Sub,
        right: Box::new(Expression::Tuple(vec![Expression::Unary(UnaryExpr {
            op: UnaryOp::Neg,
            expr: Box::new(Expression::Identifier("a".to_string()))
        })]))
    })
);

validate_expression_ast!(
    binary_sub_unary_neg_space_ast,
    "b- -a",
    Expression::Binary(BinaryExpr {
        left: Box::new(Expression::Identifier("b".to_string())),
        op: BinaryOp::Sub,
        right: Box::new(Expression::Unary(UnaryExpr {
            op: UnaryOp::Neg,
            expr: Box::new(Expression::Identifier("a".to_string()))
        }))
    })
);

validate_expression_ast!(
    binary_mul_add_ast,
    "c*a+b",
    Expression::Binary(BinaryExpr {
        left: Box::new(Expression::Binary(BinaryExpr {
            left: Box::new(Expression::Identifier("c".to_string())),
            op: BinaryOp::Mul,
            right: Box::new(Expression::Identifier("a".to_string()))
        })),
        op: BinaryOp::Add,
        right: Box::new(Expression::Identifier("b".to_string()))
    })
);

validate_expression_ast!(
    binary_add_concat_ast,
    "a+b||c",
    Expression::Binary(BinaryExpr {
        left: Box::new(Expression::Binary(BinaryExpr {
            left: Box::new(Expression::Identifier("a".to_string())),
            op: BinaryOp::Add,
            right: Box::new(Expression::Identifier("b".to_string()))
        })),
        op: BinaryOp::Concat,
        right: Box::new(Expression::Identifier("c".to_string()))
    })
);

validate_expression_ast!(
    binary_concat_compare_ast,
    "c>a||b",
    Expression::Binary(BinaryExpr {
        left: Box::new(Expression::Identifier("c".to_string())),
        op: BinaryOp::Gt,
        right: Box::new(Expression::Binary(BinaryExpr {
            left: Box::new(Expression::Identifier("a".to_string())),
            op: BinaryOp::Concat,
            right: Box::new(Expression::Identifier("b".to_string()))
        }))
    })
);

validate_expression_ast!(
    binary_compare_and_ast,
    "a<b AND c",
    Expression::Binary(BinaryExpr {
        left: Box::new(Expression::Binary(BinaryExpr {
            left: Box::new(Expression::Identifier("a".to_string())),
            op: BinaryOp::Lt,
            right: Box::new(Expression::Identifier("b".to_string()))
        })),
        op: BinaryOp::And,
        right: Box::new(Expression::Identifier("c".to_string()))
    })
);

validate_expression_ast!(
    binary_and_or_ast,
    "a AND b OR b",
    Expression::Binary(BinaryExpr {
        left: Box::new(Expression::Binary(BinaryExpr {
            left: Box::new(Expression::Identifier("a".to_string())),
            op: BinaryOp::And,
            right: Box::new(Expression::Identifier("b".to_string()))
        })),
        op: BinaryOp::Or,
        right: Box::new(Expression::Identifier("b".to_string()))
    })
);

validate_expression_ast!(
    between_ast,
    "a between b and c",
    Expression::Between(BetweenExpr {
        expr: Box::new(Expression::Identifier("a".to_string())),
        min: Box::new(Expression::Identifier("b".to_string())),
        max: Box::new(Expression::Identifier("c".to_string())),
    })
);

validate_expression_ast!(
    not_between_ast,
    "a not between b and c",
    Expression::Unary(UnaryExpr {
        op: UnaryOp::Not,
        expr: Box::new(Expression::Between(BetweenExpr {
            expr: Box::new(Expression::Identifier("a".to_string())),
            min: Box::new(Expression::Identifier("b".to_string())),
            max: Box::new(Expression::Identifier("c".to_string())),
        }))
    })
);

validate_expression_ast!(
    case_multiple_when_branches_ast,
    "case when a=b then a when c=d then c else e end",
    Expression::Case(CaseExpr {
        expr: None,
        when_branch: vec![
            WhenBranch {
                when: Box::new(Expression::Binary(BinaryExpr {
                    left: Box::new(Expression::Identifier("a".to_string())),
                    op: BinaryOp::Eq,
                    right: Box::new(Expression::Identifier("b".to_string()))
                })),
                then: Box::new(Expression::Identifier("a".to_string()))
            },
            WhenBranch {
                when: Box::new(Expression::Binary(BinaryExpr {
                    left: Box::new(Expression::Identifier("c".to_string())),
                    op: BinaryOp::Eq,
                    right: Box::new(Expression::Identifier("d".to_string()))
                })),
                then: Box::new(Expression::Identifier("c".to_string()))
            }
        ],
        else_branch: Some(Box::new(Expression::Identifier("e".to_string())))
    })
);

validate_expression_ast!(
    case_multiple_exprs_ast,
    "case a when a=b then a else c end",
    Expression::Case(CaseExpr {
        expr: Some(Box::new(Expression::Identifier("a".to_string()))),
        when_branch: vec![WhenBranch {
            when: Box::new(Expression::Binary(BinaryExpr {
                left: Box::new(Expression::Identifier("a".to_string())),
                op: BinaryOp::Eq,
                right: Box::new(Expression::Identifier("b".to_string()))
            })),
            then: Box::new(Expression::Identifier("a".to_string()))
        }],
        else_branch: Some(Box::new(Expression::Identifier("c".to_string())))
    })
);

// Group by tests
should_parse!(group_by_simple, true, "select * group by a");
should_parse!(group_by_compound, true, "select * group by a.b");
should_parse!(group_by_alias, true, "select * group by a as b");
should_parse!(
    group_by_aggregate_count_star,
    true,
    "select * group by a aggregate count(*) as b"
);
should_parse!(
    group_by_aggregate_alias,
    true,
    "select * group by a aggregate sum(a) as b"
);
should_parse!(
    group_by_aggregate_all,
    true,
    "select * group by a aggregate sum(all a) as b"
);
should_parse!(
    group_by_aggregate_distinct,
    true,
    "select * group by a aggregate sum(distinct a) as b"
);
should_parse!(
    group_by_aggregate_distinct_alias,
    true,
    "select * group by a aggregate sum(distinct a) as b"
);
should_parse!(
    group_by_aggregate_distinct_all,
    false,
    "select * group by a aggregate sum(distinct all a) as b"
);
should_parse!(group_by_none, false, "select * group by");
should_parse!(
    group_by_aggregate_none,
    false,
    "select * group by a aggregate"
);
should_parse!(
    group_by_aggregate_no_alias,
    true,
    "select * group by a aggregate sum()"
);
should_parse!(
    group_by_aggregate_no_args,
    true,
    "select * group by a aggregate sum() as b"
);

validate_query_ast!(
    group_by_aggregate_distinct_with_alias,
    "select * group by a, b aggregate sum(distinct b) as c",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: Some(GroupByClause {
            keys: vec![
                AliasedExpr {
                    expr: Expression::Identifier("a".to_string()),
                    alias: None,
                },
                AliasedExpr {
                    expr: Expression::Identifier("b".to_string()),
                    alias: None,
                }
            ],
            aggregations: vec![AliasedExpr {
                expr: Expression::Function(FunctionExpr {
                    function: FunctionName::Sum,
                    args: FunctionArguments::Args(vec![Expression::Identifier("b".to_string())]),
                    set_quantifier: Some(SetQuantifier::Distinct),
                }),
                alias: Some("c".to_string()),
            }]
        }),
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);

// Having tests
should_parse!(having_simple, true, "select * having y");
should_parse!(having_with_group_by, true, "select * group by a having y");
should_parse!(
    having_with_aggregation_function,
    true,
    "select * group by a having sum(a) > 0 "
);

validate_query_ast!(
    having_with_aggregation_distinct_and_group_by,
    "select * group by a having sum(distinct a) > 0",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: Some(GroupByClause {
            keys: vec![AliasedExpr {
                expr: Expression::Identifier("a".to_string()),
                alias: None,
            },],
            aggregations: vec![]
        }),
        having_clause: Some(Expression::Binary(BinaryExpr {
            left: Box::new(Expression::Function(FunctionExpr {
                function: FunctionName::Sum,
                args: FunctionArguments::Args(vec![Expression::Identifier("a".to_string())]),
                set_quantifier: Some(SetQuantifier::Distinct),
            })),
            op: BinaryOp::Gt,
            right: Box::new(Expression::Literal(Literal::Integer(0)))
        })),
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);

// Order by tests
should_parse!(order_by_simple_ident, true, "select * order by a");
should_parse!(order_by_compound_ident, true, "select * order by a.b");
should_parse!(order_by_asc, true, "select * order by a ASC");
should_parse!(order_by_desc, true, "select * order by a DESC");
should_parse!(order_by_multiple, true, "select a, b, c order by a, b");
should_parse!(
    order_by_multiple_directions,
    true,
    "select * order by a DESC, b ASC, c"
);
should_parse!(order_by_positional_sort, true, "select a, b order by 1, 2");
should_parse!(
    order_by_positional_sort_with_star,
    true,
    "select * order by 1"
);

validate_query_ast!(
    order_by_default_direction,
    "select * order by a",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: Some(OrderByClause {
            sort_specs: vec![SortSpec {
                key: SortKey::Simple(Expression::Identifier("a".to_string())),
                direction: SortDirection::Asc
            }]
        }),
        limit: None,
        offset: None,
    })
);

// Limit and offset tests
should_parse!(limit_simple, true, "select * limit 42");
should_parse!(offset_simple, true, "select * offset 42");
should_parse!(limit_comma_offset, true, "select * limit 42, 24");
should_parse!(limit_then_offset, true, "select * limit 42 offset 24");
should_parse!(offset_then_limit, true, "select * offset 42 limit 24");
should_parse!(offset_twice, false, "select * limit 42, 24 offset 24");
should_parse!(limit_alphabetic, false, "select * limit a");
should_parse!(limit_non_integer, false, "select * limit 42.0");
should_parse!(limit_negative, false, "select * limit -42");
should_parse!(limit_overflow, false, "select * limit 4294967296"); // 2^32

validate_query_ast!(
    limit_one_value,
    "select * limit 42",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: Some(42_u32),
        offset: None,
    })
);

validate_query_ast!(
    limit_two_values,
    "select * limit 42, 24",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: Some(42_u32),
        offset: Some(24_u32),
    })
);

validate_query_ast!(
    limit_with_offset,
    "select * limit 42 offset 24",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: Some(42_u32),
        offset: Some(24_u32),
    })
);

// Fetch first and offset tests
should_parse!(
    fetch_first_simple,
    true,
    "select * fetch first 42 rows only"
);
should_parse!(
    fetch_first_then_offset,
    true,
    "select * fetch first 42 rows only offset 24"
);
should_parse!(
    offset_then_fetch_first,
    true,
    "select * offset 42 fetch first 24 rows only"
);
should_parse!(
    fetch_first_row_synonym,
    true,
    "select * fetch first 42 row only"
);
should_parse!(fetch_next_synonym, true, "select * fetch next 42 rows only");
should_parse!(
    fetch_next_row_synonym,
    true,
    "select * fetch next 42 row only"
);
should_parse!(
    fetch_first_comma_offset,
    false,
    "select * fetch first 24 rows only, 24"
);
should_parse!(
    fetch_first_alphabetic,
    false,
    "select * fetch first a rows only"
);
should_parse!(
    fetch_first_non_integer,
    false,
    "select * fetch first 2.0 rows only"
);
should_parse!(
    fetch_first_negative,
    false,
    "select * fetch first -42 rows only"
);
should_parse!(
    fetch_first_overflow,
    false,
    "select * fetch first 4294967296 rows only"
);

validate_query_ast!(
    fetch_first_no_offset,
    "select * fetch first 42 rows only",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: Some(42_u32),
        offset: None,
    })
);

validate_query_ast!(
    fetch_first_offset,
    "select * fetch first 42 rows only offset 24",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: Some(42_u32),
        offset: Some(24_u32),
    })
);

validate_query_ast!(
    fetch_first_synonyms,
    "select * fetch next 42 row only offset 24",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: Some(42_u32),
        offset: Some(24_u32),
    })
);

should_parse!(
    order_by_neg_positional_sort,
    false,
    "select a, b order by -1, 2"
);
should_parse!(
    order_by_positional_sort_too_big,
    false,
    "select a, b order by 9223372036854775808"
);

// Literals tests
should_parse!(null_literal, true, "select null");
should_parse!(null_literal_mixed_case, true, "select nULL");
should_parse!(unsigned_int_literal, true, "select 123");
should_parse!(neg_int_literal, true, "select -123");
should_parse!(pos_int_literal, true, "select +123");
should_parse!(int_literal_leading_zeros, true, "select 008");
should_parse!(convert_to_long, true, "select 2147483648");
should_parse!(string_literal, true, "select 'foo'");
should_parse!(string_literal_special_characters, true, "select 'αβγ'");
should_parse!(empty_string_literal, true, "select ''");
should_parse!(unsigned_double_literal, true, "select 0.5");
should_parse!(unsigned_double_literal_no_fraction, true, "select 1.");
should_parse!(unsigned_double_literal_no_whole_num, true, "select .6");
should_parse!(neg_double_literal, true, "select -4.089015");
should_parse!(pos_double_literal, true, "select +0.0");
should_parse!(double_literal_exponent_lowercase, true, "select 1e2");
should_parse!(double_literal_exponent_uppercase, true, "select 2E3");
should_parse!(double_literal_exponent_beg_fraction, true, "select 8.e+23");
should_parse!(double_literal_exponent_mid_fraction, true, "select 9.07e-2");
should_parse!(double_literal_exponent_no_whole_num, true, "select .2E3");
should_parse!(double_literal_exponent_signed, true, "select -7.2E3");
should_parse!(boolean_literal_true, true, "select true");
should_parse!(boolean_literal_false, true, "select false");
should_parse!(
    boolean_literal_binary,
    true,
    "select true AND false OR false"
);

should_parse!(string_literal_single_quote, false, "select '''");
should_parse!(double_exponent_no_exp, false, "select 1e");
should_parse!(long_too_big, false, "select 9223372036854775808");

validate_expression_ast!(
    string_escaped_quote_no_chars,
    "''''",
    Expression::Literal(Literal::String(r#"'"#.to_string()))
);

validate_expression_ast!(
    string_escaped_quote,
    "'foo''s'",
    Expression::Literal(Literal::String(r#"foo's"#.to_string()))
);

validate_expression_ast!(
    double_neg_no_decimal,
    "-2E+3",
    Expression::Unary(UnaryExpr {
        op: UnaryOp::Neg,
        expr: Box::new(Expression::Literal(Literal::Double(2000.0)))
    })
);

validate_expression_ast!(
    double_no_whole,
    ".2E-3",
    Expression::Literal(Literal::Double(0.0002))
);

validate_expression_ast!(
    double_no_frac_or_sign,
    "234.E6",
    Expression::Literal(Literal::Double(234000000.0))
);

validate_expression_ast!(
    double_all_components,
    "234.2E-3",
    Expression::Literal(Literal::Double(0.2342))
);

validate_expression_ast!(
    double_binary_add,
    "2E3 + 5.16E-8",
    Expression::Binary(BinaryExpr {
        left: Box::new(Expression::Literal(Literal::Double(2000.0))),
        op: BinaryOp::Add,
        right: Box::new(Expression::Literal(Literal::Double(0.0000000516)))
    })
);

// Array tests
should_parse!(empty_array, true, "select []");
should_parse!(homogeneous_array, true, "select [1, 2, 3]");
should_parse!(heterogeneous_array, true, "select [1, 'a', true, -42]");
should_parse!(array_indexing, true, "select [1, 2, 3][0]");

// Parenthesized expressions tests
should_parse!(parens_multiple_binary_ops, true, "SELECT ((a+b)-(d/c))*7");
should_parse!(
    parens_case_expr,
    true,
    "select (CASE WHEN a=b THEN 1 ELSE 2 END)*4"
);

should_parse!(unbalanced_parens, false, "SELECT ((a+b)");
should_parse!(empty_parens, false, "SELECT ()");

// Scalar function tests
should_parse!(null_if, true, "select nullif(a, b)");
should_parse!(coalesce, true, "select coalesce(a, b, c, d)");
should_parse!(size, true, "select size(a)");
should_parse!(position, true, "select position('b' IN 'abc')");
should_parse!(position_binary_mul, true, "select position(1*2 IN 2)");
should_parse!(position_unary_neg, true, "select position(-2 IN -2)");
should_parse!(
    position_between_parens,
    true,
    "select position((b BETWEEN c AND c) IN true)"
);
should_parse!(position_binary_or, true, "select position(a OR b IN true)");
should_parse!(
    position_binary_compare,
    true,
    "select position(true IN a < b)"
);
should_parse!(char_length, true, "select char_length('foo')");
should_parse!(character_length, true, "select character_length('bar')");
should_parse!(octet_length, true, "select octet_length(a)");
should_parse!(bit_length, true, "select bit_length(a)");
should_parse!(extract_year, true, "select extract(year from a)");
should_parse!(extract_month, true, "select extract(month from a)");
should_parse!(extract_day, true, "select extract(day from a)");
should_parse!(extract_hour, true, "select extract(hour from a)");
should_parse!(extract_minute, true, "select extract(minute from a)");
should_parse!(extract_second, true, "select extract(second from a)");
should_parse!(slice_arr, true, "select slice([42, 43, 44])");
should_parse!(slice_arr_length, true, "select slice([42, 43, 44], 1)");
should_parse!(
    slice_arr_start_length,
    true,
    "select slice([42, 43, 44], 0, 1)"
);
should_parse!(
    substring_from,
    true,
    "select SUBSTRING(str FROM start FOR length)"
);
should_parse!(
    substring_comma,
    true,
    "select SUBSTRING(str, start, length)"
);
should_parse!(
    substring_comma_no_length,
    true,
    "select SUBSTRING(str, start)"
);
should_parse!(fold_upper, true, "select upper(a)");
should_parse!(fold_lower, true, "select lower(a)");
should_parse!(trim_leading, true, "select trim(LEADING substr FROM str)");
should_parse!(trim_trailing, true, "select trim(TRAILING substr FROM str)");
should_parse!(trim_both, true, "select trim(BOTH substr FROM str)");
should_parse!(current_timestamp_no_args, true, "select current_timestamp");
should_parse!(
    current_timestamp_with_args,
    true,
    "select current_timestamp(a)"
);
should_parse!(create_func_no_args, false, "select brand_new_func()");
should_parse!(
    create_func_with_args,
    false,
    "select brand_new_func(a, b, c)"
);
should_parse!(nested_scalar_func, true, "select nullif(coalesce(a, b), c)");
should_parse!(position_invalid_binary_op, false, "select position(x OR y)");
should_parse!(
    scalar_function_binary_op,
    true,
    "select char_length('foo') + 5"
);
should_fail_to_parse_with_error!(
    user_defined_function_not_allowed,
    "unknown function myFunc",
    "select myFunc(x)"
);

validate_expression_ast!(
    position_ast,
    "position((a+b*c) IN d)",
    Expression::Function(FunctionExpr {
        function: FunctionName::Position,
        args: FunctionArguments::Args(vec![
            Expression::Tuple(vec![Expression::Binary(BinaryExpr {
                left: Box::new(Expression::Identifier("a".to_string())),
                op: BinaryOp::Add,
                right: Box::new(Expression::Binary(BinaryExpr {
                    left: Box::new(Expression::Identifier("b".to_string())),
                    op: BinaryOp::Mul,
                    right: Box::new(Expression::Identifier("c".to_string()))
                }))
            })]),
            Expression::Identifier("d".to_string()),
        ]),
        set_quantifier: None,
    })
);
validate_expression_ast!(
    extract_ast,
    "extract(year from a)",
    Expression::Extract(ExtractExpr {
        extract_spec: ExtractSpec::Year,
        arg: Box::new(Expression::Identifier("a".to_string()))
    })
);
validate_expression_ast!(
    trim_default_spec,
    "trim(substr FROM str)",
    Expression::Trim(TrimExpr {
        trim_spec: TrimSpec::Both,
        trim_chars: Box::new(Expression::Identifier("substr".into())),
        arg: Box::new(Expression::Identifier("str".to_string())),
    })
);
validate_expression_ast!(
    trim_default_substr,
    "trim(leading FROM str)",
    Expression::Trim(TrimExpr {
        trim_spec: TrimSpec::Leading,
        trim_chars: Box::new(Expression::Literal(Literal::String(" ".into()))),
        arg: Box::new(Expression::Identifier("str".to_string())),
    })
);
validate_expression_ast!(
    trim_default_spec_and_substr,
    "trim(str)",
    Expression::Trim(TrimExpr {
        trim_spec: TrimSpec::Both,
        trim_chars: Box::new(Expression::Literal(Literal::String(" ".into()))),
        arg: Box::new(Expression::Identifier("str".to_string())),
    })
);
validate_expression_ast!(
    fold_ast,
    "upper(a)",
    Expression::Function(FunctionExpr {
        function: FunctionName::Upper,
        args: FunctionArguments::Args(vec![Expression::Identifier("a".to_string())]),
        set_quantifier: None,
    })
);

// From tests.
should_parse!(from_no_qualifier, true, "SELECT * FROM foo");
should_parse!(from_qualifier, true, "SELECT * FROM bar.foo");
should_parse!(from_no_qualifier_with_alias, true, "SELECT * FROM foo car");
should_parse!(from_qualifier_with_alias, true, "SELECT * FROM bar.foo car");
should_parse!(
    from_no_qualifier_with_as_alias,
    true,
    "SELECT * FROM foo AS car"
);
should_parse!(
    from_qualifier_with_as_alias,
    true,
    "SELECT * FROM bar.foo AS car"
);
should_parse!(
    from_array_with_alias,
    true,
    "SELECT * FROM [{'a': 1}, {'b': 2}] arr"
);
should_parse!(
    from_array_with_as_alias,
    true,
    "SELECT * FROM [{'a': 1}, {'b': 2}] AS arr"
);

should_parse!(
    from_two_comma_join_second_alias,
    true,
    "SELECT * FROM foo, bar AS bar"
);
should_parse!(
    from_two_comma_join_first_alias,
    true,
    "SELECT * FROM foo AS foo, bar"
);
should_parse!(
    from_two_comma_join_both_alias,
    true,
    "SELECT * FROM foo AS foo, bar AS bar"
);
should_parse!(from_two_comma_join, true, "SELECT * FROM foo, bar");
should_parse!(
    from_three_comma_join,
    true,
    "SELECT * FROM foo, bar AS bar, car"
);

should_parse!(
    from_two_inner_join_second_alias,
    true,
    "SELECT * FROM foo JOIN bar AS bar"
);
should_parse!(
    from_two_inner_join_first_alias,
    true,
    "SELECT * FROM foo AS foo INNER JOIN bar"
);
should_parse!(
    from_two_inner_join_both_alias,
    true,
    "SELECT * FROM foo AS foo INNER JOIN bar AS bar"
);
should_parse!(from_two_inner_join, true, "SELECT * FROM foo JOIN bar");

should_parse!(
    from_two_cross_join_second_alias,
    true,
    "SELECT * FROM foo CROSS JOIN bar AS bar"
);
should_parse!(
    from_two_cross_join_first_alias,
    true,
    "SELECT * FROM foo AS foo CROSS JOIN bar"
);
should_parse!(
    from_two_cross_join_both_alias,
    true,
    "SELECT * FROM foo AS foo CROSS JOIN bar AS bar"
);
should_parse!(
    from_two_cross_join,
    true,
    "SELECT * FROM foo CROSS JOIN bar"
);

should_parse!(
    from_two_left_join_second_alias,
    true,
    "SELECT * FROM foo LEFT JOIN bar AS bar"
);
should_parse!(
    from_two_left_join_first_alias,
    true,
    "SELECT * FROM foo AS foo LEFT OUTER JOIN bar"
);
should_parse!(
    from_two_left_join_both_alias,
    true,
    "SELECT * FROM foo AS foo LEFT OUTER JOIN bar AS bar"
);
should_parse!(from_two_left_join, true, "SELECT * FROM foo LEFT JOIN bar");

should_parse!(
    from_two_left_join_second_alias_with_on,
    true,
    "SELECT * FROM foo LEFT JOIN bar AS bar ON 1 = 2"
);
should_parse!(
    from_two_left_join_first_alias_with_on,
    true,
    "SELECT * FROM foo AS foo LEFT OUTER JOIN bar ON 1 = 2"
);
should_parse!(
    from_two_left_join_both_alias_with_on,
    true,
    "SELECT * FROM foo AS foo LEFT OUTER JOIN bar AS bar ON 1 = 2"
);

should_parse!(
    from_three_inner_join_with_ons,
    true,
    "SELECT * FROM foo JOIN bar ON 1 = 2 JOIN car ON 3 = 4"
);

should_parse!(
    from_two_right_join_second_alias,
    true,
    "SELECT * FROM foo RIGHT JOIN bar AS bar"
);
should_parse!(
    from_two_right_join_first_alias,
    true,
    "SELECT * FROM foo AS foo RIGHT OUTER JOIN bar"
);
should_parse!(
    from_two_right_join_both_alias,
    true,
    "SELECT * FROM foo AS foo RIGHT OUTER JOIN bar AS bar"
);
should_parse!(
    from_two_right_join,
    true,
    "SELECT * FROM foo RIGHT JOIN bar"
);

should_parse!(
    from_derived_with_alias,
    true,
    "SELECT * FROM (SELECT * FROM foo) bar"
);
should_parse!(
    from_derived_with_as_alias,
    true,
    "SELECT * FROM (SELECT * FROM foo) AS bar"
);
should_parse!(
    from_derived_must_have_alias,
    false,
    "SELECT * FROM (SELECT * FROM foo)"
);

validate_query_ast!(
    from_comma_join_is_cross_join,
    "SELECT * FROM foo, bar",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: Some(Datasource::Join(JoinSource {
            join_type: JoinType::Cross,
            left: Box::new(Datasource::Collection(CollectionSource {
                database: None,
                collection: "foo".to_string(),
                alias: None
            })),
            right: Box::new(Datasource::Collection(CollectionSource {
                database: None,
                collection: "bar".to_string(),
                alias: None
            })),
            condition: None
        })),
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
validate_query_ast!(
    from_cross_join_is_cross_join,
    "SELECT * FROM foo CROSS JOIN bar",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: Some(Datasource::Join(JoinSource {
            join_type: JoinType::Cross,
            left: Box::new(Datasource::Collection(CollectionSource {
                database: None,
                collection: "foo".to_string(),
                alias: None
            })),
            right: Box::new(Datasource::Collection(CollectionSource {
                database: None,
                collection: "bar".to_string(),
                alias: None
            })),
            condition: None
        })),
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
validate_query_ast!(
    from_join_is_cross_join,
    "SELECT * FROM foo JOIN bar",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: Some(Datasource::Join(JoinSource {
            join_type: JoinType::Cross,
            left: Box::new(Datasource::Collection(CollectionSource {
                database: None,
                collection: "foo".to_string(),
                alias: None
            })),
            right: Box::new(Datasource::Collection(CollectionSource {
                database: None,
                collection: "bar".to_string(),
                alias: None
            })),
            condition: None
        })),
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
validate_query_ast!(
    from_left_join_is_left_join,
    "SELECT * FROM foo LEFT JOIN bar",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: Some(Datasource::Join(JoinSource {
            join_type: JoinType::Left,
            left: Box::new(Datasource::Collection(CollectionSource {
                database: None,
                collection: "foo".to_string(),
                alias: None
            })),
            right: Box::new(Datasource::Collection(CollectionSource {
                database: None,
                collection: "bar".to_string(),
                alias: None
            })),
            condition: None
        })),
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
validate_query_ast!(
    from_right_join_is_right_join,
    "SELECT * FROM foo RIGHT JOIN bar",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: Some(Datasource::Join(JoinSource {
            join_type: JoinType::Right,
            left: Box::new(Datasource::Collection(CollectionSource {
                database: None,
                collection: "foo".to_string(),
                alias: None
            })),
            right: Box::new(Datasource::Collection(CollectionSource {
                database: None,
                collection: "bar".to_string(),
                alias: None
            })),
            condition: None
        })),
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
validate_query_ast!(
    from_join_is_left_associative,
    "SELECT * FROM foo JOIN bar JOIN car",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: Some(Datasource::Join(JoinSource {
            join_type: JoinType::Cross,
            left: Box::new(Datasource::Join(JoinSource {
                join_type: JoinType::Cross,
                left: Box::new(Datasource::Collection(CollectionSource {
                    database: None,
                    collection: "foo".to_string(),
                    alias: None,
                })),
                right: Box::new(Datasource::Collection(CollectionSource {
                    database: None,
                    collection: "bar".to_string(),
                    alias: None,
                })),
                condition: None
            })),
            right: Box::new(Datasource::Collection(CollectionSource {
                database: None,
                collection: "car".to_string(),
                alias: None
            })),
            condition: None
        })),
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);

should_fail_to_parse_with_error!(
    from_cannot_have_more_than_one_qualifier,
    "collection data sources can only have database qualification, found: car.bar.foo",
    "SELECT * FROM car.bar.foo"
);
should_fail_to_parse_with_error!(
    from_cannot_be_document,
    "found unsupported expression used as datasource: {'foo': 3 + 4}",
    "SELECT * FROM {'foo': 3+4}"
);
should_fail_to_parse_with_error!(
    from_cannot_be_literal,
    "found unsupported expression used as datasource: 3",
    "SELECT * FROM 3"
);
should_fail_to_parse_with_error!(
    from_cannot_be_binary_op,
    "found unsupported expression used as datasource: 3 + 4",
    "SELECT * FROM 3 + 4"
);
should_fail_to_parse_with_error!(
    from_array_must_have_alias,
    "array datasources must have aliases",
    "SELECT * FROM [{'a': 1}]"
);

should_parse!(where_single_condition, true, "select * WHERE a >= 2");
should_parse!(where_single_column_expr, true, "select * WHERE a");
should_parse!(
    where_multiple_conditions,
    true,
    "select * WHERE a > 1 AND b > 1"
);
should_parse!(
    where_case_expr,
    true,
    "select * WHERE CASE WHEN a = true THEN a ELSE false END"
);
should_parse!(where_null, true, "select * WHERE NULL");

validate_query_ast!(
    where_ast,
    "SELECT * WHERE a >= 2",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Star])
        },
        from_clause: None,
        where_clause: Some(Expression::Binary(BinaryExpr {
            left: Box::new(Expression::Identifier("a".to_string())),
            op: BinaryOp::Gte,
            right: Box::new(Expression::Literal(Literal::Integer(2)))
        })),
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);

// Type conversion and assertion tests
should_parse!(cast_to_double, true, "select CAST(v AS DOUBLE)");
should_parse!(
    cast_to_double_precision,
    true,
    "select CAST(v AS DOUBLE PRECISION)"
);
should_parse!(cast_to_double_shorthand, true, "select v::DOUBLE PRECISION");
should_parse!(cast_to_real, true, "select CAST(v AS REAL)");
should_parse!(cast_to_real_shorthand, true, "select v::REAL");
should_parse!(cast_to_float, true, "select CAST(v AS FLOAT)");
should_parse!(cast_to_float_int, true, "select CAST(v AS FLOAT(25))");
should_parse!(cast_to_float_shorthand, true, "select v::FLOAT(25)");
should_parse!(cast_to_string, true, "select CAST(v AS STRING)");
should_parse!(cast_to_string_shorthand, true, "select v::STRING");
should_parse!(cast_to_varchar, true, "select CAST(v AS VARCHAR)");
should_parse!(cast_to_varchar_int, true, "select CAST(v AS VARCHAR(1))");
should_parse!(cast_to_varchar_shorthand, true, "select v::VARCHAR(1)");
should_parse!(cast_to_char, true, "select CAST(v AS CHAR)");
should_parse!(cast_to_char_int, true, "select CAST(v AS CHAR(1))");
should_parse!(cast_to_char_shorthand, true, "select v::CHAR(1)");
should_parse!(cast_to_character, true, "select CAST(v AS CHARACTER)");
should_parse!(
    cast_to_character_int,
    true,
    "select CAST(v AS CHARACTER(1))"
);
should_parse!(cast_to_character_shorthand, true, "select v::CHARACTER(1)");
should_parse!(cast_to_char_varying, true, "select CAST(v AS CHAR VARYING)");
should_parse!(
    cast_to_char_varying_int,
    true,
    "select CAST(v AS CHAR VARYING(1))"
);
should_parse!(
    cast_to_char_varying_shorthand,
    true,
    "select v::CHAR VARYING(1)"
);
should_parse!(
    cast_to_character_varying,
    true,
    "select CAST(v AS CHARACTER VARYING)"
);
should_parse!(
    cast_to_character_varying_int,
    true,
    "select CAST(v AS CHARACTER VARYING(1))"
);
should_parse!(
    cast_to_character_varying_shorthand,
    true,
    "select v::CHARACTER VARYING(1)"
);
should_parse!(cast_to_document, true, "select CAST(v AS DOCUMENT)");
should_parse!(cast_to_document_shorthand, true, "select v::DOCUMENT");
should_parse!(cast_to_array, true, "select CAST(v AS ARRAY)");
should_parse!(cast_to_array_shorthand, true, "select v::ARRAY");
should_parse!(cast_to_bindata, true, "select CAST(v AS BINDATA)");
should_parse!(cast_to_bindata_shorthand, true, "select v::BINDATA");
should_parse!(cast_to_undefined, true, "select CAST(v AS UNDEFINED)");
should_parse!(cast_to_undefined_shorthand, true, "select v::UNDEFINED");
should_parse!(cast_to_object_id, true, "select CAST(v AS OBJECTID)");
should_parse!(cast_to_object_id_shorthand, true, "select v::OBJECTID");
should_parse!(cast_to_bool, true, "select CAST(v AS BOOL)");
should_parse!(cast_to_bool_shorthand, true, "select v::BOOL");
should_parse!(cast_to_bit, true, "select CAST(v AS BIT)");
should_parse!(cast_to_bit_shorthand, true, "select v::BIT");
should_parse!(cast_to_boolean, true, "select CAST(v AS BOOLEAN)");
should_parse!(cast_to_boolean_shorthand, true, "select v::BOOLEAN");
should_parse!(cast_to_bson_date, true, "select CAST(v AS BSON_DATE)");
should_parse!(cast_to_bson_date_shorthand, true, "select v::BSON_DATE");
should_parse!(cast_to_timestamp, true, "select CAST(v AS TIMESTAMP)");
should_parse!(cast_to_timestamp_shorthand, true, "select v::TIMESTAMP");
should_parse!(cast_to_null, true, "select CAST(v AS NULL)");
should_parse!(cast_to_null_shorthand, true, "select v::NULL");
should_parse!(cast_to_regex, true, "select CAST(v AS REGEX)");
should_parse!(cast_to_regex_shorthand, true, "select v::REGEX");
should_parse!(cast_to_dbpointer, true, "select CAST(v AS DBPOINTER)");
should_parse!(cast_to_dbpointer_shorthand, true, "select v::DBPOINTER");
should_parse!(cast_to_javascript, true, "select CAST(v AS JAVASCRIPT)");
should_parse!(cast_to_javascript_shorthand, true, "select v::JAVASCRIPT");
should_parse!(cast_to_symbol, true, "select CAST(v AS SYMBOL)");
should_parse!(cast_to_symbol_shorthand, true, "select v::SYMBOL");
should_parse!(
    cast_to_javascriptwithscope,
    true,
    "select CAST(v AS JAVASCRIPTWITHSCOPE)"
);
should_parse!(
    cast_to_javascriptwithscope_shorthand,
    true,
    "select v::JAVASCRIPTWITHSCOPE"
);
should_parse!(cast_to_int, true, "select CAST(v AS INT)");
should_parse!(cast_to_int_shorthand, true, "select v::INT");
should_parse!(cast_to_integer, true, "select CAST(v AS INTEGER)");
should_parse!(cast_to_integer_shorthand, true, "select v::INTEGER");
should_parse!(cast_to_small_int, true, "select CAST(v AS SMALLINT)");
should_parse!(cast_to_small_int_shorthand, true, "select v::SMALLINT");
should_parse!(
    cast_to_bson_timestamp,
    true,
    "select CAST(v AS BSON_TIMESTAMP)"
);
should_parse!(
    cast_to_bson_timestamp_shorthand,
    true,
    "select v::BSON_TIMESTAMP"
);
should_parse!(cast_to_long, true, "select CAST(v AS LONG)");
should_parse!(cast_to_long_shorthand, true, "select v::LONG");
should_parse!(cast_to_decimal, true, "select CAST(v AS DECIMAL)");
should_parse!(cast_to_decimal_int, true, "select CAST(v AS DECIMAL(1))");
should_parse!(cast_to_decimal_shorthand, true, "select v::DECIMAL(1)");
should_parse!(cast_to_dec, true, "select CAST(v AS DEC)");
should_parse!(cast_to_dec_int, true, "select CAST(v AS DEC(1))");
should_parse!(cast_to_dec_shorthand, true, "select v::DEC");
should_parse!(cast_to_numeric, true, "select CAST(v AS NUMERIC)");
should_parse!(cast_to_numeric_int, true, "select CAST(v AS NUMERIC(1))");
should_parse!(cast_to_numeric_shorthand, true, "select v::NUMERIC");
should_parse!(cast_to_minkey, true, "select CAST(v AS MINKEY)");
should_parse!(cast_to_minkey_shorthand, true, "select v::MINKEY");
should_parse!(cast_to_maxkey, true, "select CAST(v AS MAXKEY)");
should_parse!(cast_to_maxkey_shorthand, true, "select v::MAXKEY");
should_parse!(cast_on_null, true, "select CAST(v AS BOOL, 'null' ON NULL)");
should_parse!(
    cast_on_error,
    true,
    "select CAST(v AS INT, 'null' ON ERROR)"
);
should_parse!(
    cast_on_null_on_error,
    true,
    "select CAST(v AS INT, 'null' ON NULL, 'error' ON ERROR)"
);
should_parse!(type_assert, true, "select a::!STRING");
should_parse!(
    type_assert_in_func,
    true,
    "select SUBSTRING(foo::!STRING, 1, 1)"
);

validate_expression_ast!(
    cast_to_decimal_ast,
    "CAST(v AS DECIMAL(1), 'null' ON NULL, 'error' ON ERROR)",
    Expression::Cast(CastExpr {
        expr: Box::new(Expression::Identifier("v".to_string())),
        to: Type::Decimal128,
        on_null: Some(Box::new(Expression::Literal(Literal::String(
            "null".to_string()
        )))),
        on_error: Some(Box::new(Expression::Literal(Literal::String(
            "error".to_string()
        )))),
    })
);
validate_expression_ast!(
    cast_precedence_binary,
    "a * b::int",
    Expression::Binary(BinaryExpr {
        left: Box::new(Expression::Identifier("a".to_string())),
        op: BinaryOp::Mul,
        right: Box::new(Expression::Cast(CastExpr {
            expr: Box::new(Expression::Identifier("b".to_string())),
            to: Type::Int32,
            on_null: None,
            on_error: None,
        }))
    })
);
validate_expression_ast!(
    cast_precedence_unary,
    "NOT a::bool",
    Expression::Unary(UnaryExpr {
        op: UnaryOp::Not,
        expr: Box::new(Expression::Cast(CastExpr {
            expr: Box::new(Expression::Identifier("a".to_string())),
            to: Type::Boolean,
            on_null: None,
            on_error: None,
        }))
    })
);

// Subquery tests
should_parse!(simple_subquery, true, "SELECT VALUE (SELECT a)");
should_parse!(multiple_nested_subqueries, true, "SELECT (SELECT a)");
should_parse!(exists_subquery, true, "SELECT EXISTS (SELECT a)");
should_parse!(not_exists_subquery, true, "SELECT NOT EXISTS (SELECT a)");
should_parse!(any_subquery, true, "SELECT x <> ANY (SELECT a)");
should_parse!(all_subquery, true, "SELECT x = ALL (SELECT a)");
should_parse!(in_tuple_subquery, true, "SELECT X IN (A, B, C)");
should_parse!(not_in_tuple_subquery, true, "SELECT X NOT IN (A, B, C)");

should_parse!(in_test, true, "SELECT X NOT IN (1+2-3)");

should_parse!(empty_tuple, false, "SELECT X NOT IN ()");
should_parse!(tuple_with_dangling_comma, false, "SELECT X NOT IN (A,)");

validate_expression_ast!(
    some_subquery,
    "x <> SOME (SELECT a)",
    Expression::SubqueryComparison(SubqueryComparisonExpr {
        expr: Box::new(Expression::Identifier("x".to_string())),
        op: BinaryOp::Neq,
        quantifier: SubqueryQuantifier::Any,
        subquery: Box::new(Query::Select(SelectQuery {
            select_clause: SelectClause {
                set_quantifier: SetQuantifier::All,
                body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                    expr: Expression::Identifier("a".to_string()),
                    alias: None
                })])
            },
            from_clause: None,
            where_clause: None,
            group_by_clause: None,
            having_clause: None,
            order_by_clause: None,
            limit: None,
            offset: None
        }))
    })
);

validate_expression_ast!(
    in_subquery,
    "x IN (SELECT a)",
    Expression::Binary(BinaryExpr {
        left: Box::new(Expression::Identifier("x".to_string())),
        op: BinaryOp::In,
        right: Box::new(Expression::Subquery(Box::new(Query::Select(SelectQuery {
            select_clause: SelectClause {
                set_quantifier: SetQuantifier::All,
                body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                    expr: Expression::Identifier("a".to_string()),
                    alias: None
                })])
            },
            from_clause: None,
            where_clause: None,
            group_by_clause: None,
            having_clause: None,
            order_by_clause: None,
            limit: None,
            offset: None
        }))))
    })
);

validate_expression_ast!(
    not_in_subquery,
    "x NOT IN (SELECT a)",
    Expression::Binary(BinaryExpr {
        left: Box::new(Expression::Identifier("x".to_string())),
        op: BinaryOp::NotIn,
        right: Box::new(Expression::Subquery(Box::new(Query::Select(SelectQuery {
            select_clause: SelectClause {
                set_quantifier: SetQuantifier::All,
                body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                    expr: Expression::Identifier("a".to_string()),
                    alias: None
                })])
            },
            from_clause: None,
            where_clause: None,
            group_by_clause: None,
            having_clause: None,
            order_by_clause: None,
            limit: None,
            offset: None
        }))))
    })
);

// Document and field-access expression tests
macro_rules! map(
  { $($key:expr => $value:expr),+ } => {
    {
      let mut m = LinkedHashMap::new();
      $(
        m.insert($key, $value);
      )+
      m
    }
  };
);

should_parse!(empty_doc_literal, true, "select {}");
should_parse!(doc_literal, true, "select {'a':1, 'b':2}");
should_parse!(doc_mixed_field_binary_op, true, "select {'a':3+4}");
should_parse!(doc_field_access_bracket, true, "select doc['a']");
should_parse!(doc_literal_field_access_dot, true, "select {'a': 1}.a");
should_parse!(
    doc_literal_field_access_multi_level_dot,
    true,
    "select {'a': {'b': {'c': 100}}}.a.b.c"
);
should_parse!(
    doc_literal_field_dot_star_field,
    true,
    "select {'a': {'*': 100, 'b': 10, 'c': 1}}.a.`*`"
);
should_parse!(
    doc_literal_field_access_bracket,
    true,
    "select {'a': 1, 'b': 2}['a']"
);
should_parse!(
    doc_literal_field_access_bracket_one_level,
    true,
    "select a['b']"
);
should_parse!(
    doc_literal_field_access_bracket_multi_level,
    true,
    "select a['b']['c']"
);
should_parse!(
    doc_literal_field_bracket_star_field,
    true,
    "select {'a': {'*': 100, 'b': 10, 'c': 1}}['a']['*']"
);

should_parse!(doc_literal_non_string_keys, false, "select {1:1, 2:2}");
should_parse!(non_doc_field_access, false, "select 1.a");

validate_expression_ast!(
    doc_literal_field_access_ast,
    "{'a': {'b': {'c': 100}}}.a.b.c",
    Expression::Subpath(SubpathExpr {
        expr: Box::new(Expression::Subpath(SubpathExpr {
            expr: Box::new(Expression::Subpath(SubpathExpr {
                expr: Box::new(Expression::Document(
                    map! {"a".to_string() => Expression::Document(
                        map!{"b".to_string() => Expression::Document(
                            map!{"c".to_string() => Expression::Literal(Literal::Integer(100))}
                        )}
                    )}
                )),
                subpath: "a".to_string()
            })),
            subpath: "b".to_string()
        })),
        subpath: "c".to_string()
    })
);
validate_expression_ast!(
    doc_mixed_field_access,
    "a.b['c'].d",
    Expression::Subpath(SubpathExpr {
        expr: Box::new(Expression::Access(AccessExpr {
            expr: Box::new(Expression::Subpath(SubpathExpr {
                expr: Box::new(Expression::Identifier("a".to_string())),
                subpath: "b".to_string()
            })),
            subfield: Box::new(Expression::Literal(Literal::String("c".to_string()))),
        })),
        subpath: "d".to_string()
    })
);

// Comment tests
should_parse!(
    comment_standard_parse,
    true,
    "SELECT a FROM foo -- This is a standard comment"
);
should_parse!(
    comment_standard_conflict_with_minus_minus,
    true,
    "SELECT a -- b"
);
should_parse!(
    comment_standard_single_line_parse,
    true,
    "-- This is a standard single line comment
    SELECT a FROM foo"
);
should_parse!(
    comment_inline_parse,
    true,
    "SELECT a /* This is an inline comment */ FROM foo"
);
should_parse!(
    comment_multiline_parse,
    true,
    "/* This is a multiline comment
    This is a multiline comment */
    SELECT a FROM foo"
);
should_parse!(
    comment_multiline_inline_parse,
    true,
    "SELECT a /* This is an inline
    comment */ FROM foo"
);
should_parse!(
    comment_multiline_nesting,
    true,
    "/* This is a multiline comment
    * with nesting: /* nested block comment */
    */
    SELECT a FROM foo"
);

validate_query_ast!(
    comment_standard_ast,
    "SELECT foo -- This is a standard comment",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier("foo".to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);

validate_query_ast!(
    comment_ast,
    "-- This is a standard single line comment
    SELECT foo",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier("foo".to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);

validate_query_ast!(
    comment_inline_ast,
    "SELECT /* This is an inline comment */ foo",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier("foo".to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);

validate_query_ast!(
    comment_multiline_ast,
    "/* This is a multiline comment
    This is a multiline comment */
    SELECT foo",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier("foo".to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);

validate_query_ast!(
    comment_inline_multiline_ast,
    "SELECT /* This is an inline
    comment */ foo",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier("foo".to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);

validate_query_ast!(
    comment_multiline_nesting_ast,
    "/* This is a multiline comment
    * with nesting: /* nested block comment */
    */
    SELECT foo",
    Query::Select(SelectQuery {
        select_clause: SelectClause {
            set_quantifier: SetQuantifier::All,
            body: SelectBody::Standard(vec![SelectExpression::Aliased(AliasedExpr {
                expr: Expression::Identifier("foo".to_string()),
                alias: None
            })])
        },
        from_clause: None,
        where_clause: None,
        group_by_clause: None,
        having_clause: None,
        order_by_clause: None,
        limit: None,
        offset: None,
    })
);
