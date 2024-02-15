/************************************************************************************************
File Name: parser.h
Compiler : MS Visual Studio 2019
Author: Jason Waid & Joshua Whiting
Course: Compilers CST8152
Assignment #: 3
Date: December 5th 2019
Professor: Sv. Ranev
Description: Parser implementation for the PLAYPUS compilier
Function List: parser(),  match(), syn_eh(), syn_printe(), gen_incode(), program(), opt_statements(),
statements(), statement(), statement_primary(), assignment_statement(), assignment_expression(),
selection_statement(), iteration_statement(), pre_condition(), input_statement(),
variable_identifier(), variable_list(), variable_list_primary(), output_statement(), opt_variable_list(),
arithmetic_expression(), unary_arithmetic_expression(), additive_arithmetic_expression(),
 additive_arithmetic_expression_primary(), multiplicative_arithmetic_expression(),
 multiplicative_arithmetic_expression_primary(), primary_arithmetic_expression(),
 string_expression(), string_expression_primary(), primary_string_expression(),
 conditional_expression(), logical_OR_expression(), logical_OR_expression_primary(),
 logical_AND_expression(), logical_AND_expression_primary(), relational_expression(),
 primary_s_relational_expression(), primary_s_relational_expression_primary(),
 primary_a_relational_expression(), primary_a_relational_expression_primary()
**************************************************************************************************************/

#pragma once
#ifndef PARSER_H_
#define PARSER_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

static Token lookahead;
int synerrno;

#define NO_ATTR -1
#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

extern int line;
extern pBuffer str_LTBL;
extern char* kw_table[];
extern Token malar_next_token();

void parser();
void match(int, int);
void syn_eh(int);
void syn_printe();
void gen_incode(char*);
void program();
void opt_statements();
void statements();
void statement();
void statement_primary();
void assignment_statement();
void assignment_expression();
void selection_statement();
void iteration_statement();
void pre_condition();
void input_statement();
void variable_identifier();
void variable_list();
void variable_list_primary();
void output_statement();
void opt_variable_list();
void arithmetic_expression();
void unary_arithmetic_expression();
void additive_arithmetic_expression();
void additive_arithmetic_expression_primary();
void multiplicative_arithmetic_expression();
void multiplicative_arithmetic_expression_primary();
void primary_arithmetic_expression();
void string_expression();
void string_expression_primary();
void primary_string_expression();
void conditional_expression();
void logical_OR_expression();
void logical_OR_expression_primary();
void logical_AND_expression();
void logical_AND_expression_primary();
void relational_expression();
void primary_s_relational_expression();
void primary_s_relational_expression_primary();
void primary_a_relational_expression();
void primary_a_relational_expression_primary();

#endif