 /**********************************************************************************************************
* File Name:	table.h
* Compiler:		MS Visual Studio 2019
* Author:		Svillen Ranev (unfinished)
* Completed by: Joshua Whiting & Jason Waid
* Course:		CST 8152 - Compilers, Lab Section: 012, 023
* Assignment:	2
* Date:			11/15/2019
* Professor:	Sv.Ranev
* Purpose:		Transition Table and function declarations necessary for the scanner implementation
* Function list: aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func10(), aa_func11()
**********************************************************************************************************/
#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol*/
#define SEOF '\0'

#define ES  11 /* Error state  with no retract */
#define ER  11 /* Error state  with retract */
#define IS -1    /* Invalid state */

/* State transition table definition 
*/
#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
int  st_table[ ][TABLE_COLUMNS] = {
/*            [a-zA-Z] , 0 , [1-9] , . , @ ,other, ", SEOF                            */
/* State 0 */   {1,      6,    4,   ER, ER,   ER,  9,  IS},
/* State 1 */   {1,      1,    1,    2,  3,   2,   2,  ER},
/* State 2*/	{IS,    IS,   IS,   IS, IS,   IS,  IS, IS},
/* State 3*/	{IS,    IS,   IS,   IS, IS,   IS,  IS, IS},
/* State 4*/	{ES,     4,    4,    7,  5,    5,   5, ER},
/* State 5*/	{IS,    IS,   IS,   IS, IS,   IS,  IS, IS},
/* State 6*/	{ES,     6,   ES,    7, ES,    5,   5, ER},
/* State 7*/	{ 8,     7,    7,    8,  8,    8,   8, ER},
/* State 8*/	{IS,    IS,   IS,   IS, IS,   IS,  IS, IS},
/* State 9*/	{ 9,     9,    9,    9,  9,    9,  10, ER},
/* State 10*/	{IS,    IS,   IS,   IS, IS,   IS,  IS, IS},
/* State 11*/	{IS,    IS,   IS,   IS, IS,   IS,  IS, IS},
/* State 12*/	{IS,    IS,   IS,   IS, IS,   IS,  IS, IS}
};
 
/* Accepting state table definition*/
#define ASWR     1  /* accepting state with retract */
#define ASNR     3  /* accepting state with no retract */
#define NOAS     0  /* not accepting state */

/*YOUR INITIALIZATION HERE - USE ASWR, ASNR, NOAS */
int as_table[ ] = {
/* State 0 */  NOAS,
/* State 1 */  NOAS,
/* State 2*/	ASWR,
/* State 3*/	ASNR,
/* State 4*/	NOAS,
/* State 5*/	ASWR,
/* State 6*/	NOAS,
/* State 7*/	NOAS,
/* State 8*/	ASWR,
/* State 9*/	NOAS,
/* State 10*/	ASNR,
/* State 11*/	ASNR,
/* State 12*/	ASWR
/* State 13*/					
};
/* Accepting action function declarations */

Token aa_func02(char *lexeme); //AVID /KW
Token aa_func03(char *lexeme); //SVID
Token aa_func05(char *lexeme); //DIL
Token aa_func08(char *lexeme); //FPL
Token aa_func10(char *lexeme); //SL
Token aa_func11(char *lexeme); //ES



/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
PTR_AAF aa_table[ ] = {
/* State 0 */   NULL,
/* State 1 */   NULL,
/* State 2*/	aa_func02,
/* State 3*/	aa_func03,
/* State 4*/	NULL,
/* State 5*/	aa_func05,
/* State 6*/	NULL,
/* State 7*/	NULL,
/* State 8*/	aa_func08,
/* State 9*/	NULL,
/* State 10*/	aa_func10,
/* State 11*/	aa_func11,
/* State 12     aa_func12	*/
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table []=
	{
	"ELSE", /*0*/
	"FALSE", /*1*/
	"IF", /*2*/
	"PLATYPUS", /*3*/
	"READ", /*4*/
	"REPEAT", /*5*/
	"THEN", /*6*/
	"TRUE", /*7*/
	"WHILE", /*8*/
	"WRITE"   /*9*/
	};

#endif
                     