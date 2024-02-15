/************************************************************************************************
File Name: parser.c
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


#include <stdlib.h>
#include "parser.h"

/**********************************************************************************************************
Provided by:		Svillen Ranev
Called Function:	malar_next_token(), program(), match(), get_incode()
Parameters:			void
Return Value:		NAN
Algorithm:			Starts the process of parsing the source file
**********************************************************************************************************/
void parser(void) {
	lookahead = malar_next_token();
	program(); 
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");

}

/**********************************************************************************************************
Partial Code:		Svillen Ranev
Completed By:		Joshua Whiting
Called Function:	syn_eh(), malar_next_token(), syn_printe();
Parameters:			int pr_token_code, int pr_token_attribute
Return Value:		NAN
Algorithm:			Matches the token with the correct attribute
**********************************************************************************************************/
void match(int pr_token_code, int pr_token_attribute) {
	
	
	if (lookahead.code != pr_token_code) { /*if no matches are found, print error token*/
		syn_eh(pr_token_code);
		return;
	}

	if (lookahead.code == SEOF_T) { /*checks for SEOF_T token*/
		return;
	}
	switch (pr_token_code) /*checks for matching token*/
	{
	case KW_T:

	///*	if (pr_token_attribute != lookahead.attribute.kwt_idx) { /*if the attribute doesnt match with the token, print error token*/
	//		syn_eh(pr_token_code);
	//		return;
	//	}
	//	break;
		
	case LOG_OP_T:
	case ART_OP_T:
	case REL_OP_T:

		if (pr_token_attribute != lookahead.attribute.get_int) { /*if the attribute doesnt match with the token, print error token*/
			syn_eh(pr_token_code);
			return;
		}
		break;
	default:
		break;
	}
	lookahead = malar_next_token(); /*checks for next token*/

	if (lookahead.code == ERR_T) { /*if the next token is error, calls the error printing method*/
		syn_printe();
		lookahead = malar_next_token();
		synerrno++;
		return;
	}

	
}

/**********************************************************************************************************
Partial Code:		Svillen Ranev
Completed By:		Joshua Whiting
Called Function:	syn_printe(),  malar_next_token(), exit()
Parameters:			int sync_token_code
Return Value:		NAN
Algorithm:			Keeps track of the total number of syntax errors
**********************************************************************************************************/
void syn_eh(int sync_token_code) {

	syn_printe(); /*calling the syn_printe function*/
	synerrno++; /*error counter*/

	while (lookahead.code != sync_token_code) {
			
		lookahead = malar_next_token();

		if (lookahead.code == SEOF_T) {
			if (sync_token_code != SEOF_T) {
				exit(synerrno);
			}
			else
			{
			return;
			}
		}
	}
		lookahead = malar_next_token();
		return;
}


/**********************************************************************************************************
Partial Code:		Svillen Ranev
Completed By:		Joshua Whiting
Called Function:	printf()
Parameters:			NAN
Return Value:		NAN
Algorithm:			Prints the syntax errors
**********************************************************************************************************/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/
/**********************************************************************************************************
Partial Code:		Svillen Ranev
Completed By:		Joshua Whiting
Called Function:	printf()
Parameters:			char* string
Return Value:		NAN
Algorithm:			Prints the syntax errors
**********************************************************************************************************/
void gen_incode(char* string) {
	printf("%s\n",string);
}

/**********************************************************************************************************
Partial Code:		Svillen Ranev
Completed By:		Joshua Whiting
Called Function:	match(), opt_statements(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<program>  ->
  									PLATYPUS {<opt_statements>} 
					FIRST(<program>) = {KW_T(PLATYPUS)}
**********************************************************************************************************/
void program() {
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}
/**********************************************************************************************************
Author:				Svillen Ranev
Called Function:	statements(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			
					<output statement> ->
										WRITE (<opt_variable_list>);| WRITE (STR_T);
					FIRST(<output statement>) = {KW_T(WRITE)}
**********************************************************************************************************/
void opt_statements() {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statements();
			break;
		}
		break;
	default:
		gen_incode("PLATY: Opt_statements parsed");
		
	}
}


/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	match(), opt_statements(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<statements> -> 
									<statement><statements’>
					FIRST(<statements>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ)}
**********************************************************************************************************/
void statements() {
	statement();
	statement_primary();
}

/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	assignment_statement(), selection_statement(), iteration_statement(), input_statement()
					output_statement(), syn_printe()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<statement> ->
									<assignment statement>|<selection statement>|<iteration statement>
 									|<input statement>|<output statement>
					FIRST(<statement>) = {AVID_T,SVID_T,KW_T(READ),KW_T(WRITE),KW_T(WHILE),KW_T(IF)}

**********************************************************************************************************/
void statement() {
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		assignment_statement(); 
		break;
	case KW_T:
		switch (lookahead.attribute.get_int) {
		case IF:
			selection_statement(); break;
		case WHILE:
			iteration_statement(); break;
		case READ:
			input_statement(); break;
		case WRITE:
			output_statement(); break;
		default:
			syn_printe();
			break;
		}
		break;
	default:
		syn_printe();
		break;
	}
}

/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	statement(), statement_primary()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<statements’>-> 
									<statement><statements’> | ɛ
					FIRST(<statements’>) = {AVID_T,SVID_T,KW_T(READ),KW_T(WRITE),KW_T(WHILE),KW_T(IF),ɛ}
**********************************************************************************************************/
void statement_primary() {
	switch (lookahead.code)
	{
	case KW_T:
		switch (lookahead.attribute.get_int) {
		case READ:
		case WRITE:
		case WHILE:
		case REPEAT:
		case IF:
			statement(); 
			statement_primary();
			break;
		default:
			break;
		}
		break;
	case AVID_T:
	case SVID_T:
		statement();
		statement_primary();
		break;
	default:
		break;
	}
}
/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	assignment_expression(), match(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<assignment statement> -> 
												<assignment expression>;
					FIRST(<assignment statement>)={FIRST(<assignment expression>)} = {AVID,SVID}
**********************************************************************************************************/
void assignment_statement() {
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	match(), arithmetic_expression(), gen_incode(), string_expression() syn_printe()
Parameters:			NAN
Return Value:		NAN
Algorithm:			< assignment expression> -> 
												AVID = <arithmetic expression>|SVID = <string expression>
					FIRST(<assignment expression>) = {AVID,SVID}
**********************************************************************************************************/
void assignment_expression() {
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default:
		syn_printe();
		break;
	}
}
/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	primary_string_expression(), string_expression_primary(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<string expression> ->
											<string expression’><primary string expression>
					FIRST(<string expression>) = {FIRST(primary string expression>)} = {SVID_T, STR_T}
**********************************************************************************************************/
void string_expression() {	
	primary_string_expression();
	string_expression_primary();
	gen_incode("PLATY: String expression parsed");
}
/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	match(), primary_string_expression() string_expression_primary()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<string expression’> ->
											<string expression’> << <primary string expression> | ɛ
					FIRST (<string expression’>) = {SCC_OP_T, ɛ}
**********************************************************************************************************/
void string_expression_primary() {
	switch (lookahead.code)
	{
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_primary();
		break;
	default:
		break;
	}
}
/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	match(), primary_string_expression() string_expression_primary()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<primary string expression> ->
													SVID_T|STR_T
					FIRST(<primary string expression>) = {SVID_T, STR_T};
**********************************************************************************************************/
void primary_string_expression() {
	switch (lookahead.code)
	{
	case SVID_T:
	case STR_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary string expression parsed");
}
/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	unary_arithmetic_expression(), syn_printe(), additive_arithmetic_expression(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<arithmetic expression> - >
												<unary arithmetic expression>  |<additive arithmetic expression>	
					FIRST(<arithmetic expression>) = {ART_OP_T(PLUS),ART_OP_T(MINUS),AVID_T,FPL_T,INL_T}		
**********************************************************************************************************/
void arithmetic_expression() {
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
		case MINUS:
			unary_arithmetic_expression();
				break;
		default: syn_printe();
			break;
		}
		break;
	case AVID_T:
	case FPL_T:
	case LPR_T:
	case INL_T:
		additive_arithmetic_expression();
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}
/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	match(), primary_arithmetic_expression(), match(), syn_printe(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<unary arithmetic expression> ->
													 -  <primary arithmetic expression> | + <primary arithmetic expression>
					FIRST(<unary arithmetic expression>) =	{ART_OP_T(PLUS),ART_OP_T(MINUS)}

**********************************************************************************************************/
void unary_arithmetic_expression() {
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
			match(ART_OP_T, PLUS);
			primary_arithmetic_expression();
			break;
		case MINUS:
			match(ART_OP_T, MINUS);
			primary_arithmetic_expression();
			break;
		default:
			syn_printe();
			break;
		}
		break;
	default:
		syn_printe();
		break;
	}
	
	gen_incode("PLATY: Unary arithmetic expression parsed");
}
/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	match(), arithmetic_expression(), match(), syn_printe(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<primary arithmetic expression> ->
														AVID_T| FPL_T| INL_T| (<arithmetic expression>)	
					FIRST(<primary arithmetic expression>) = {AVID_T,FPL_T,INL_T,LPR_T}

**********************************************************************************************************/
void primary_arithmetic_expression() {
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}
/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	match(), arithmetic_expression(), match(), syn_printe(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<additive arithmetic expression> -> 
														<multiplicative arithmetic expression><additive arithmetic expression’>
					FIRST(<additive arithmetic expression>) = {AVID_T, FPL_T, INL_T,LPR_T,ART_OP_T(DIV),ART_OP_T(MULTI),ɛ }

**********************************************************************************************************/
void additive_arithmetic_expression() {
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_primary();
}

/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	match(), multiplicative_arithmetic_expression(), gen_incode() , additive_arithmetic_expression_primary()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<additive arithmetic expression’> ->
														+ <multiplicative arithmetic expression><additive arithmetic expression’>| -  <multiplicative arithmetic expression><additive arithmetic expression’>| e
					FIRST (<additive arithmetic expression’>) = { +, -, e}
**********************************************************************************************************/
void additive_arithmetic_expression_primary() {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			match(ART_OP_T, lookahead.attribute.arr_op);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_primary();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		default:
				break;
		}
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	default:
		break;
	}
}
/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	primary_arithmetic_expression(), multiplicative_arithmetic_expression_primary()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<multiplicative arithmetic expression> ->
																<multiplicative arithmetic expression’><primary arithmetic expression>
					FIRST(<multiplicative arithmetic expression>) = {AVID_T, FPL_T, INL_T, LPR_T};
**********************************************************************************************************/
void multiplicative_arithmetic_expression() {
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_primary();
}
/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	primary_arithmetic_expression(), multiplicative_arithmetic_expression_primary(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<multiplicative arithmetic expression’> ->
																<multiplicative arithmetic expression’>* | <multiplicative arithmetic expression’> / | ɛ
					FIRST(<multiplicative arithmetic expression’>) = {ART_OP_T(DIV), ART_OP_T(MULT), ɛ}
**********************************************************************************************************/
void multiplicative_arithmetic_expression_primary() {
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case DIV:
		case MULT:
			match(ART_OP_T, lookahead.attribute.arr_op);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_primary();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		default:
			
			break;
		}
		break;
	
	default:
	break;
	}
}
/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	match(), pre_condition(), conditional_expression(), opt_statements(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<selection statement> ->
												IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> } ELSE { <opt_statements> } 
					FIRST(<selection statement >)= {KW_T(IF)}
**********************************************************************************************************/
void selection_statement() {
	match(KW_T, IF);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}
/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	match(), syn_printe()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<pre-condition> ->
								TRUE | FALSE
					FIRST(<pre-condition>) = {KW_T(TRUE),KW_T(FALSE)}

**********************************************************************************************************/
void pre_condition() {
	switch (lookahead.code) {
	case KW_T:
		switch (lookahead.attribute.get_int) {
		case TRUE:
			match(KW_T, TRUE);
			break;
		case FALSE:
			match(KW_T, FALSE);
			break;

		default:
			syn_printe();
			break;
		}
		break;
	default:
		syn_printe();
		break;
	}
}

/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	logical_OR_expression(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:	 <conditional expression> -> <logical OR  expression>
			  FIRST (<conditional expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
**********************************************************************************************************/
void conditional_expression() {
	logical_OR_expression();		
	gen_incode("PLATY: Conditional expression parsed");

}
/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	logical_AND_expression(), logical_OR_expression_primary()
Parameters:			NAN
Return Value:		NAN
Algorithm:	<logical OR expression> -> <logical AND expression> <logical OR expression>
			FIRST (<logical OR expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
**********************************************************************************************************/
void logical_OR_expression() {
	logical_AND_expression();	
	logical_OR_expression_primary();
}
/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	match(), logical_AND_expression(), logical_OR_expression_primary(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:	<logical OR expression> -> .OR.  <logical AND expression><logical OR expression> | e
			 FIRST (<logical OR expression>) = { .OR. , ɛ }
**********************************************************************************************************/
void logical_OR_expression_primary() {
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case OR:
			match(LOG_OP_T, OR);
			logical_AND_expression();
			logical_OR_expression_primary();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		}
		break;
	default:
		break;
	}
}
/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	relational_expression(), logical_AND_expression_primary()
Parameters:			NAN
Return Value:		NAN
Algorithm: <logical AND expression> -> <relational expression> <logical AND expression>
					  FIRST (<logical AND expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
**********************************************************************************************************/
void logical_AND_expression() {
	relational_expression();
	logical_AND_expression_primary();

}
/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	match(), relational_expression(), logical_AND_expression_primary(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:	<logical AND expression> -> .AND.  <relational expression><logical AND expression> | ɛ
					  FIRST (<logical AND expression>) = {.AND. , ɛ }
**********************************************************************************************************/
void logical_AND_expression_primary() {

	switch (lookahead.code) {

	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case AND:
		match(LOG_OP_T, AND);
		relational_expression();
		logical_AND_expression_primary();
		gen_incode("PLATY: Logical AND expression parsed");
		break;
		default:
			break;
		}
		break;
	default:
		break;
	}
}

/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	match(), opt_variable_list(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:	
		<output statement> ->
				WRITE (<opt_variable_list>) | WRITE (STR_T)
				FIRST(<output statement>) = { KW_T(WRITE) }
				FIRST(<opt_variable_list>) = {AVID_T,SVID_T,STR_T, ɛ}
**********************************************************************************************************/
void output_statement() {
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	opt_variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");

}

/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	variable_list(), variable_list(), match(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:	 <output_variable list> -> <variable list> | STR_T;
			FIRST (<output_list>) = {AVID_T, SVID_T, STR_T, ɛ}
**********************************************************************************************************/
void opt_variable_list() {
	switch (lookahead.code) {

	case AVID_T:
		/*Variable Value*/
		variable_list();
		break;

	case SVID_T:
		/*Variable*/
		variable_list();
		break;

	case STR_T:
		/*String Literal*/
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;

	default:
		/*epsilon*/
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}
}
/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	primary_a_relational_expression(), primary_a_relational_expression_primary()
					primary_s_relational_expression(), primary_s_relational_expression_primary()
					syn_printe(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:	 <relational expression> ->
				  <primary a_relational expression> <primary  a_relational expression>
				| <primary s_relational  expression> <primary s_relational expression>				  
				FIRST(<relational expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
**********************************************************************************************************/
void relational_expression() {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		primary_a_relational_expression_primary();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		primary_s_relational_expression_primary();
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Relational expression parsed");

}
/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	match(), syn_printe(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:	<primary a_relational expression> ->
									AVID_T  | FPL_T  | INL_T
					  FIRST(<primary a_relational expression>)= {  AVID_T, FPL_T, INL_T}
**********************************************************************************************************/
void primary_a_relational_expression() {

	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	default:
		syn_printe();
		break;

	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	match(), primary_a_relational_expression(), primary_a_relational_expression(), 
					syn_printe()
Parameters:			NAN
Return Value:		NAN
Algorithm:	<primary a_relational expression> ->
							 == <primary a_relational expression>
						   | <> <primary a_relational expression>
						   | > <primary a_relational expression>
						   | < <primary a_relational expression>
					  FIRST(<primary a_relational expression>) = { ==, <>, >, < }
**********************************************************************************************************/
void primary_a_relational_expression_primary() {
	switch (lookahead.code) {
	case REL_OP_T:
		switch (lookahead.attribute.rel_op) {
		
		case EQ:
			match(REL_OP_T, EQ);
			primary_a_relational_expression();
			break;
		case NE:
			match(REL_OP_T, NE);
			primary_a_relational_expression();
			break;
		case GT:
			match(REL_OP_T, GT);
			primary_a_relational_expression();
			break;
		case LT:
			match(REL_OP_T, LT);
			primary_a_relational_expression();
			break;

		default:
			syn_printe();
			break;
		}
		break;
	default:
		syn_printe();
		break;
	}

}

/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	primary_string_expression(), gen_incode()
					syn_printe()
Parameters:			NAN
Return Value:		NAN
Algorithm:	<primary s_relational expression> -> <primary string expression>
				 FIRST(<primary s_relational expression >)= { SVID_T, STR_T }
**********************************************************************************************************/
void primary_s_relational_expression() {
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/**********************************************************************************************************
Completed By:		Jason Waid
Called Function:	match(), primary_s_relational_expression(), syn_printe()
Parameters:			NAN
Return Value:		NAN
Algorithm:	<primary s_relational expression> ->
						  == <primary s_relational expression>
						| <> <primary s_relational expression>
						| > <primary s_relational expression>
						| < <primary s_relational expression>
					  FIRST(<primary s_relational expression>) = { ==, <>, >, <  }
**********************************************************************************************************/
void primary_s_relational_expression_primary() {

	switch (lookahead.code) {
	case REL_OP_T:
		switch (lookahead.attribute.rel_op) {
		case EQ:
			match(REL_OP_T, EQ);
			primary_s_relational_expression();
			break;

		case NE:
			match(REL_OP_T, NE);
			primary_s_relational_expression();
			break;

		case GT:
			match(REL_OP_T, GT);
			primary_s_relational_expression();
			break;

		case LT:
			match(REL_OP_T, LT);
			primary_s_relational_expression();
			break;

		default:
			syn_printe();
			break;
		}
		break;
	default:
		syn_printe();
		break;
	}

}

/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	match(), pre_condition(), conditional_expression(), statements(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:	<iteration statement> ->
          WHILE <pre-condition> (<conditional expression>)
          REPEAT { <statements>};
		FIRST(<iteration statement>) ={KW_T(WHILE)}

**********************************************************************************************************/
void iteration_statement() {
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}
/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	match(), variable_list(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:			<input statement> ->
					READ (<variable list>);
					FIRST(<input statement>)={KW_T(READ)}
**********************************************************************************************************/
void input_statement() {
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	variable_identifier(), variable_list_primary(), gen_incode()
Parameters:			NAN
Return Value:		NAN
Algorithm:	<variable list> ->
		    <variable list’><variable identifier>
			FIRST(<variable list>) = {AVID_T,SVID}
**********************************************************************************************************/
void variable_list() {	
	variable_identifier();
	variable_list_primary();
	gen_incode("PLATY: Variable list parsed");
}

/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	match(), variable_identifier(), variable_list_primary()
Parameters:			NAN
Return Value:		NAN
Algorithm:	<variable list’> ->
			<variable identifier><variable list’>|ɛ
			FIRST(<variable list’>) = {AVID_T,SVID,ɛ}

**********************************************************************************************************/
void variable_list_primary() {
	switch (lookahead.code)
	{
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_primary();
		break;
	default:
		break;
	}
}
/**********************************************************************************************************
Completed By:		Joshua Whiting
Called Function:	match(), syn_printe()
Parameters:			NAN
Return Value:		NAN
Algorithm: 
			<variable identifier>
			FIRST(<variable identifier>) = {AVID_T, SVID_T}
		
**********************************************************************************************************/
void variable_identifier() {
	switch (lookahead.code)
	{		
	case AVID_T:
	case SVID_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}

}