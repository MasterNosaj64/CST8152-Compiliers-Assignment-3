/**********************************************************************************************************
* File Name:	scanner.c
* Compiler:		MS Visual Studio 2019
* Author:		Jason Waid & Joshua Whiting
* Course:		CST 8152 - Compilers, Lab Section: 012
* Assignment:	2
* Date:			11/15/2019
/* Professor:	Sv.Ranev
* Purpose:		Implementation of a Scanner for the PLATYPUS language
* Function list:smalar_next_token(), get_next_state(), char_class() ,aa_function02(),aa_function03(),aa_function05(),
		aa_function08(), aa_function10(), aa_function11(),iskeyword();
**********************************************************************************************************/
/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static int iskeyword(char* kw_lexeme); /*keywords lookup functuion */

/**********************************************************************************************************
Author:				Svillen Ranev
Called Function:	b_clear(), b_rewind(), b_isempty()
Parameters:			psc_buf	-pBuffer	pointer to buffer structure
					
Return Value:		EXIT_SUCCESS | 	EXIT_FAILURE

Algorithm:			Checks status of the buffer
					if empty returns failure
					Otherwise rewinds the buffer and clears the string literal buffer
**********************************************************************************************************/
int scanner_init(pBuffer psc_buf) {
  	if(b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  no need - global ANSI C */
}

/**********************************************************************************************************
Author:				Jason Waid
Called Function:	b_getc(), b_retract(), b_mark(), b_reset(), b_getcoffset(), b_location(), b_allocate(),
					b_addc(), b_compact(), b_free(), aa_func11(), isalpha(), isdigit()
Parameters:			void
Return Value:		t-	Token
Algorithm:			checks every character by value and validates if it is a part of the PLATYPUS language
					When a token is found its attributes are assigned and it is returned to calling function
**********************************************************************************************************/
Token malar_next_token(void) {
		Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
		unsigned char c; /* input symbol */
		int state = 0; /* initial state of the FSM */
		short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
		short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
		
		while (1) { /* endless loop broken by token returns it will generate a warning */

		/*GET THE NEXT SYMBOL FROM THE INPUT BUFFER */

			c = b_getc(sc_buf);

			switch (c) {

				/*Check for Source-End-Of-File token*/
			case '\0':
				t.code = SEOF_T;
				t.attribute.seof = SEOF_EOF;
				return t;
				break;
			case '0xFF': case 255: case EOF:
				t.code = SEOF_T;
				t.attribute.seof = SEOF_0;
				return t;
				break;

				/*Check for white space*/
			case ' ':
				continue;

				/*Check for tab character*/
			case '\t':
				continue;

				/*Check for form feed*/
			case '\f':
				break;

				/*vertible tab*/
			case '\v':
				break;
				/*Carriage return*/
			case '\r':
				line++;
				continue;

				/*Check for new line*/
			case '\n':
				line++;
				continue;

				/*Check for semi colon*/
			case ';':
				t.code = EOS_T;
				return t;

				/*Check for coma*/
			case ',':
				t.code = COM_T;
				return t;

			case '=':

				c = b_getc(sc_buf);
				if (c == '=') { /*if another '=' is found it must be comparative =*/

					t.code = REL_OP_T;
					t.attribute.rel_op = EQ;
					return t;
				}
				else /* otherwise its just regular equals*/
				{
					b_retract(sc_buf); /*place the character we pulled out of the buffe rback into the buffer*/
					t.code = ASS_OP_T;
					return t;
				}

				/*Check for Left parenthesis*/
			case '(':
				t.code = LPR_T;
				return t;

				/*Checks for right parenthesis*/
			case ')':
				t.code = RPR_T;
				return t;

				/*Check for left brace*/
			case '{':
				t.code = LBR_T;
				return t;

				/*Check for right brace*/
			case '}':
				t.code = RBR_T;
				return t;

				/*Check for grless than OR not equal to sign*/
			case '<':

				c = b_getc(sc_buf);
				if (c == '>') {/* checks for not equal to token*/

					t.code = REL_OP_T;
					t.attribute.rel_op = NE;
					return t;
				}
				else if (c == '<') {/*checks for String concatenation*/
					t.code = SCC_OP_T;
return t;
				}
				else
				{
				b_retract(sc_buf);/*place the character we pulled out of the buffe rback into the buffer*/
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;
				}

				/*check for greater than sign*/
			case '>':

				t.code = REL_OP_T;
				t.attribute.rel_op = GT;
				return t;

				/**********************************************
				 ********** Arithmetic Operators *************
				 ***********************************************/

				 /*Check for plus operator*/
			case '+':

				/*maybe add a check for another + sign*/
				t.code = ART_OP_T;
				t.attribute.arr_op = PLUS;
				return t;

				/*Check for minus operator*/
			case '-':

				/* maybe add check for another - sign*/

				t.code = ART_OP_T;
				t.attribute.arr_op = MINUS;
				return t;

				/*Check for divide operator*/
			case '/':
				t.code = ART_OP_T;
				t.attribute.arr_op = DIV;
				return t;

				/*Check for Multi operator*/
			case '*':
				t.code = ART_OP_T;
				t.attribute.arr_op = MULT;
				return t;


				/*********************************************
				 **************Logical Operators************
				 *******************************************/

			case '.':
				b_mark(sc_buf, b_getcoffset(sc_buf));
				c = b_getc(sc_buf);

				if (c == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.') {

					t.code = LOG_OP_T;
					t.attribute.log_op = AND;
					return t;

				}
				else if (c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.') {
					t.code = LOG_OP_T;
					t.attribute.log_op = OR;
					return t;
				}
				else
				{
					t.code = ERR_T;
					t.attribute.err_lex[0] = '.';
					t.attribute.err_lex[1] = '\0';

					b_reset(sc_buf);
					return t;

				}
				break;

				/*************************************************
				 *****************Comment Syntax*****************
				 **********************************************/


				 /*Check for exclamation point*/
			case '!':

				/*checking for next character after !*/
				c = b_getc(sc_buf);

				if (c == '!') {

					/*attempt to process comment
					does so by skipping tokens until end of line is read*/
					while (c != '\n') {

						c = b_getc(sc_buf);
						if (c == '\0' || c == EOF || c == 255 || c == '0xFF'){
							t.code = ERR_T;
							return t;
						}
					} 
					/*Added to while loop above, added handling of EOF characters \0 and EOF*/
					line++;
					continue;

				}
				else
				{
					/*error because comments are suposed to be '!!' not '!'*/
					t.code = ERR_T;

					/*String for error message*/
					/* Where the error begins*/
					t.attribute.err_lex[0] = '!';
					/*what we're taking in*/
					t.attribute.err_lex[1] = c;
					/*end the string created will null terminating byte*/
					t.attribute.err_lex[2] = '\0';

					/*consume all of the characters until the new line character*/
					while (c != '\n' && c != '\r' && c != '\0') {
						c = b_getc(sc_buf);
					}
					line++;
					return t;
				}

				/**********************************************************
				 ************FINITE STATE MACHINE****************
				 *******************************************************/

			default:


				if (isalpha(c) || isdigit(c) || c == '"') {

					b_retract(sc_buf);
					lexstart = b_getcoffset(sc_buf);
					b_mark(sc_buf, lexstart);
					c = b_getc(sc_buf);

					while (as_table[state] == NOAS){
						state = get_next_state(state, c);

						if (as_table[state] != NOAS) {
							break;
						}
						c = b_getc(sc_buf);
					}
					//possibly might run into a new line issue where new line is encourntered but the line counter is not incremented appropietly 
					if(as_table[state] == ASWR){
							b_retract(sc_buf);

					}

					/*reached accepting state*/
					lexstart = b_mark(sc_buf, lexstart);
					lexend = b_getcoffset(sc_buf);


					lex_buf = b_allocate((lexend - lexstart) + 1, 0, 'f');

					if (lex_buf == NULL) {
						t.code = ERR_T;
						scerrnum = 1;
						aa_func11("RUN TIME ERROR: ");
						return t;
					}

					b_reset(sc_buf);


					while (b_getcoffset(sc_buf) != lexend)
					{
						c = b_getc(sc_buf);
						b_addc(lex_buf, c);
					}
					b_compact(lex_buf, '\0');
					t = aa_table[state](b_location(lex_buf));

					b_free(lex_buf);
					return t;
				}
				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = '\0';
				return t;
			}
			return t;
		}/*end while(1)*/
}

/**********************************************************************************************************
Author:				Svillen Ranev
Called Function:	char_class(), assert()
Parameters:			state	-int	the current state of the scanner
					c		-char	the current character loaded into the scanner

Return Value:		next the next state

Algorithm:			Gets the next state for the scanner to use
**********************************************************************************************************/
int get_next_state(int state, char c)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif

assert(next != IS);

#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif

return next;
}
/***********************************************************************
* Purpose: Returns the column number in the transition table 
			st_table for the input characater c
* Author:              Joshua Whiting
* History/Versions:    1.0
* Called functions: isalpha(), isdigit()
* Parameters: char c
*
* Return value: int val
* Algorithm:  Compares character c with st_table and returns the value 
				of the column
**********************************************************************/
int char_class (char c)
{
        int val = 5;
        if(isalpha(c)){ /* Col 0 [a-zA-Z]*/
          val = 0;
        }else if(c =='0'){ /*Col 1 0 */
          val = 1;
        }else if(isdigit(c)){ /*Col 2 [1-9]*/
          val = 2;
        }else if(c == '.'){ /*Col 3 . */
          val = 3;
        }else if(c == '@'){ /*Col 4 @ */
          val = 4;
        }else if(c == '"'){ /*Col 6 " */
          val = 6;
        }else if(c == '\0'){ /*Col 7 SEOF */
          val = 7;
        }else{ /* Col 5 other */
          val = 5;
        }

        return val;
}
/***********************************************************************
* Purpose:	Accepting function for arithmentic variable and keywords
			(VID - AVID/KW)
* Author:              Joshua Whiting
* History/Versions:    1.0
* Called functions: iskeyword(),strlen(),strncpy()
* Parameters: char lexeme[]
*
* Return value: Token t
* Algorithm: Checks if lexeme is keyword, if yes. returns the keyword
			and sets the attribute.
			If not, it sets an avid token and adds \0 at the end to make 
			c-type string
**********************************************************************/
Token aa_func02(char lexeme[]){
  Token t = {0};
  int index = (int)iskeyword(lexeme); /*checks for keyword*/

  if (index != -1) { /*if keyword is found*/
	  t.code = KW_T;
	  t.attribute.kwt_idx = index;
  }
  else {
	  short lexlength = (short)strlen(lexeme);
	  if (lexlength > VID_LEN)
		  lexlength = VID_LEN;
	  /*Set Token*/
	  t.code = AVID_T;
	  strncpy(t.attribute.vid_lex, lexeme, lexlength);
	  t.attribute.vid_lex[lexlength] = '\0';
  }
  return t;
}
/***********************************************************************
* Purpose: Accepting function for the string variable (VID - SVID)
* Author:              Joshua Whiting
* History/Versions:    1.0
* Called functions: strlen()
* Parameters: char lexeme[]
* Return value: Token t
* Algorithm: Sets a SVID Token, Checks if the lexeme is longer than VID_LEN
			 stores the lexeme into the token's attribute vid_lex
			 Adds \0 to create string
* Return value: t - Token
* Algorithm:
**********************************************************************/
Token aa_func03(char lexeme[]){
  Token t = {0};
  int size = strlen(lexeme);


  if(size > VID_LEN){
	  size = VID_LEN;
  }    
  t.code = SVID_T;

  strncpy(t.attribute.vid_lex, lexeme, size - 1);
  t.attribute.vid_lex[size - 1] = '@';
  t.attribute.vid_lex[size] = '\0';

  return t;
}

/***********************************************************************
* Purpose: Converts the lexeme to a floating point
* Author:              Joshua Whiting
* History/Versions:    1.0
* Called functions: atof(), strlen()
* Parameters: char lexeme[]
* Return value: token t
* Algorithm: converts lexeme into floating point value,
			 error checking, making sure its in range and same size of
			 a 4 byte float sets the token attribute flt value and 
			 token code
**********************************************************************/
Token aa_func08(char lexeme[]){
  Token t = {0};
  double fpv = atof(lexeme);
  if((fpv < FLT_MIN || fpv > FLT_MAX) && (fpv >= 0 && strlen(lexeme) > 7)){
    t = aa_table[11](lexeme);
  }else{
    t.code = FPL_T;
    t.attribute.flt_value = (float) fpv;
  }

  return t;
}

/***********************************************************************
* Purpose: Converts the lexeme into a decimal integer value
* Author:              Joshua Whiting
* History/Versions:    1.0
* Called functions: atoi(), strlen()
* Parameters: char lexeme[]
*
* Return value: token t
* Algorithm: converts lexeme into decimal integer,
			 error checking, making sure integar is the size of 2 byte,
			 and in range of INL_Len,
			 then sets token code and attribute int value
**********************************************************************/
Token aa_func05(char lexeme[]){
  Token t = {0};
  int toDec = atoi(lexeme);

  if(toDec < SHRT_MIN || toDec > SHRT_MAX || strlen(lexeme) > INL_LEN){
    t = aa_table[11](lexeme);
  }else{
    t.code = INL_T;
  	t.attribute.int_value = toDec;
  }

  return t;
}

/***********************************************************************
* Purpose:		Function loops through stirng literal lexme and adds it 
				to str_LTBL buf
* Author:              Jason Waid
* History/Versions:    1.0
* Called functions:	b_limit(), b_addc(), strlen()
* Parameters:	lexeme[] -char a Lexeme string literal
*
* Return value:	token
* Algorithm:	iterates through every character while validating each 
				character to detmine the size of the strling literal buffer
**********************************************************************/
Token aa_func10(char lexeme[]){

  Token t = {0};
  int i;
  t.attribute.str_offset = b_limit(str_LTBL);

  for (i = 0; i < (int) strlen(lexeme); i++){

    if(lexeme[i] != '"'){
      b_addc(str_LTBL, lexeme[i]);
    }

    if(lexeme[i] == '\n'){
      line++;
    }
 
  }
  b_addc(str_LTBL, '\0');

  t.code = STR_T;
  return t;
}

/***********************************************************************
* Purpose: Sets the error token.
* Author:              Jason waid
* History/Versions:    1.0
* Called functions: strlen()
* Parameters: char lexeme[]
*
* Return value: Token t
* Algorithm: iterates through the passed lexeme when errors occur,
iterates tru and adds three . at the end
**********************************************************************/
Token aa_func11(char lexeme[]) {
	Token t = { 0 };
	int lexLen;
	int count;
	t.code = ERR_T;

	lexLen = strlen(lexeme);/*get length of lexme string*/

	for (count = 0; count < ERR_LEN - 3; count++)
	{
		if (count <= lexLen - 1)
		{
			if (lexeme[count] == '\n' || lexeme[count] == '\r')
			{
				line++;
			}
			t.attribute.err_lex[count] = lexeme[count];
		}
		else
		{
			break;
		}
	}
		if (lexLen > ERR_LEN - 1) {
			t.attribute.err_lex[count] = '.';
			t.attribute.err_lex[++count] = '.';
			t.attribute.err_lex[++count] = '.';
			t.attribute.err_lex[++count] = '\0';
		}
		t.attribute.err_lex[count] = '\0';
		return t;
}
/***********************************************************************
* Purpose: Iterates through a loop and tries to find a keyword in the kw_lexeme array            
* Author:              Joshua Whiting
* History/Versions:    1.0
* Called functions:   strcmp()
* Parameters: char* kw_lexeme         
*					   
* Return value: int       
* Algorithm: checks if kw_lexeme is empty, if empty, return -1. 
*			 Loops through the table size and checks if the keyword matches one in the table
*			 If found, returns the index, if not, returns -1
**********************************************************************/
int iskeyword(char* kw_lexeme){
  int iterator = 0;

  if(!kw_lexeme){ /*checks if kw_lexeme is empty*/
    return RT_FAIL_1;
  }

  for(iterator; iterator < KWT_SIZE; iterator++){ /*loops through table*/
    if(strcmp(kw_table[iterator],kw_lexeme) == 0){
      return iterator;
    }
  }
/* keyword not found, return -1*/
  return -1;
}