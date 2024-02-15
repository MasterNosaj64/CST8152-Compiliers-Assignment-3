#include "buffer.h"
/**********************************************************************************************************
* File Name:	buffer.c
* Compiler:		MS Visual Studio 2019
* Author:		Jason Waid
* Course:		CST 8152 - Compilers, Lab Section: 012
* Assignment:	1
* Date:			10/06/2019
/* Professor:	Sv.Ranev
* Purpose:		Contains the functions of a buffer to create and manipulate the buffer
* Function list:malloc(), calloc(), realloc(), b_allocate(), b_addc(), b_clear(),
*               b_free(), b_isfull(), b_limit(), b_capacity(),
*				b_mark(), b_mode(), b_incfactor(), b_load(),
*               b_isempty(), b_getc(), b_eob(), b_print(), b_compact(), b_rflag(),
*				b_retract(), b_reset(), b_getcoffset(), b_rewind(), b_location().
**********************************************************************************************************/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer* const pBD);
void b_free(Buffer* const pBD);
int b_isfull(Buffer* const pBD);
short b_limit(Buffer* const pBD);
short b_capacity(Buffer* const pBD);
short b_mark(pBuffer const pBD, short mark);
int b_mode(Buffer* const pBD);
size_t b_incfactor(Buffer* const pBD);
int b_load(FILE* const fi, Buffer* const pBD);
int b_isempty(Buffer* const pBD);
char b_getc(Buffer* const pBD);
int b_eob(Buffer* const pBD);
int b_print(Buffer* const pBD, char nl);
Buffer* b_compact(Buffer* const pBD, char symbol);
char b_rflag(Buffer* const pBD);
short b_retract(Buffer* const pBD);
short b_reset(Buffer* const pBD);
short b_getcoffset(Buffer* const pBD);
int b_rewind(Buffer* const pBD);
char* b_location(Buffer* const pBD);

/**********************************************************************************************************
Purpose:			Creates the buffer structure in the heap(dynamically) while populating 
					the structures data members appropietly, this is all done without crashes
					or memory leaks
Author:				Jason Waid
History/Versions:	10/05/2019
Called Function:	malloc(), b_free(), calloc()
Parameters:			init_capacity	-short	Must be within range of
											short min and short max.
											Must be possitive.
					inc_factor		-char	Inclusive range between 0 to 255
											Must be positive.
					o_mode			-char	must be "f,a,m".
Return Value:		pBuffer		*Buffer	that points to a valid Buffer structure

Algorithm:			Check all parameters for errors.
					If init_capacity is greater than Short_max-1 or less than zero return null
					Check calloc/malloc for errors, returns null if error found
					Check o_mode for mode of buffer opperation

					If init_capacity is less than or equal to short_max-1 and greater than or equal to 0 apply the following logic

					If init_capacity is 0:

							Check mode,
								if o_mode is "a" (additive) or "m" (multiplicative) assign buffer inc_factor to default value of zero
								if o_mode is "f" (fixed) assign inc_factor to 0
								The above also updates mode data on buffer.
								-1 = Mulitplicative mode
								0 = Fixed Mode
								1 = Additive Mode

								Following this apply default inc_factor of 15
								Following this apply the default capacity of 200

					If init_capacity isn't 0 apply the following logic
						check mode,
							if o_mode is "f" (fixed) and inc_factor is equal to zero and init_capaict is not equal to zero
								buffer inc_factor is assigned to zero

							if o_mode is "a" (additive) and inc_factor is greater than or equal to 1 and less than or equal to 255
								buffer inc_factor is passed via method args

							if o_mode is "m" (multiplicative) and inc_factor is greater than or equal to 1 and less than or equal to 100
								buffer inc_factor is passed via method args
						
						mode field of buffer is updated
								-1 = Mulitplicative mode
								0 = Fixed Mode
								1 = Additive Mode
				
				If the above results in a error all memory is free'd before returning null to avoid memory leaks and pointers are set to null.
				Following the above Defaults flags are assigned to the buffer for determining end of buffer and memory realocation

				A pointer to a Buffer struct is then returned to the calling function
**********************************************************************************************************/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode)
{
	/*Corrected Typo for inc_factor parameter Oct 9th 2019*/
	Buffer* pBuffer;
	/*Attempt to allocated memory for buffer struct*/
	pBuffer = (Buffer*) calloc(1, sizeof(Buffer));
	/*if memory allocation failed return NULL to report error to calling method*/
	if (pBuffer == NULL) {
		return NULL;
	}

/*If init_capacity is within range of data type max (SHRT_MAX-1) and 0 inclusive apply following logic*/
	if (init_capacity <= MAXIMUM_BUFFER_CAPACITY && init_capacity >= 0) {
		
		/*if init_capacity equals 0, use the default size and following logic*/
		if (init_capacity == 0) {
			/*Allocates memory for Character array using default size of 200 chars*/
			pBuffer->cb_head = (char*)malloc(sizeof(char)*DEFAULT_INIT_CAPACITY);
			
			/*if memory allocation for cb_head failed return NULL to report error to calling function*/
			if (pBuffer->cb_head == NULL) {
				return NULL;
			}
			/*IF operation mode is Additive or Multiplicative apply the following logic*/
			if (o_mode == ADD_MODE || o_mode == MULTI_MODE) {
				
				/*Applies increment factor to default value of 15 ONLY when init_capaicty equal 0 and 
				operation mode is either Additive or Multiplicative*/
				pBuffer->inc_factor = (unsigned char) DEFAULT_INC_FACTOR;
				
				/*if buffer mode is Additive Self-Incrementing*/
				if (o_mode == ADD_MODE) {
					pBuffer->mode = ADD_MODE_NUM;
				}
				else {/*otherwise buffer mode is Multiplicative Self-Incrementing*/
					pBuffer->mode = MULTI_MODE_NUM;
				}
			}/*If operation mode is Fixed mode assign 0 to increment factor*/
			else if (o_mode == FIXED_MODE) {
				pBuffer->mode = FIXED_MODE_NUM;
				pBuffer->inc_factor = 0;
			}
			else {/*If the above conditions for operation mode did not match
				clear memory and resolve dangling pointers, return NUll to report error to calling function*/
				
				/*free memory to avoid any memory leaks*/
				b_free(pBuffer);
				/*Assign pointer to NULL to avoid dangling pointers*/
				pBuffer = NULL;
				return NULL;
			}
			
			/*assigns current capacity of buffer as default initial capacity*/
			pBuffer->capacity = DEFAULT_INIT_CAPACITY;
		}
		else {
			/*If init_capacity is not equal to zero, but is still within range of 0 and SHRT_MAX-1
			apply the following logic*/
			
			/*Allocates memory for Character array using desired capacity*/
			pBuffer->cb_head = (char*)malloc(sizeof(char)*init_capacity);
			
			/*if memory allocation for cb_head failed return NULL to report error to calling function*/
			if (pBuffer->cb_head == NULL) {
				return NULL;
			}

			/*if operation mode (o_mode)is FIXED assign mode to 0 and inc_factor to 0, ignoring passed inc_factor.*/
			/*Corrected Typo for inc_factor parameter Oct 9th 2019*/
			if ((o_mode == FIXED_MODE) | ((unsigned char) inc_factor == 0 && init_capacity != 0)) {
				pBuffer->mode = FIXED_MODE_NUM;
				pBuffer->inc_factor = 0;
			}
			/*if operation mode (o_mode) is Additive and within range of 255 and 1 inclusive
			assign mode to 1 and inc_factor to passed increment factor*/
			/*Corrected Typo for inc_factor parameter Oct 9th 2019*/
			else if (o_mode == ADD_MODE && (unsigned char) inc_factor >= 1 && (unsigned char) inc_factor <= 255) {
				pBuffer->mode = ADD_MODE_NUM;
				pBuffer->inc_factor = (unsigned char) inc_factor;
			}
			/*if operation mode (o_mode) is Multiplicative and within range of 100 and 1 inclusive
			assign mode to 1 and inc_factor to passed increment factor*/
			/*Corrected Typo for inc_factor parameter Oct 9th 2019*/
			else if (o_mode == MULTI_MODE && (unsigned char) inc_factor >= 1 && (unsigned char) inc_factor <= 100) {
				pBuffer->mode = MULTI_MODE_NUM;
				pBuffer->inc_factor = (unsigned char) inc_factor;
			}else{/*If the above conditions for operation mode and inc_factor did not match
				clear memory and resolve dangling pointers, return NUll to report error to calling function*/
				
				/*free memory to avoid any memory leaks*/
				b_free(pBuffer);
				/*Assign pointer to NULL to avoid dangling pointers*/
				pBuffer = NULL;
				return NULL;
			}
			/*assign buffer capacity to passed capacity (init_capacity) if no error has happened*/
			pBuffer->capacity = init_capacity;
		}
		/*Set buffer flags to their defualt values if not errors occured*/
		pBuffer->flags = DEFAULT_FLAGS;
	}
	else {/*if passed capacity (init_capacity is less than 0 or greater than SHRT_MAX -1
		  return NULL to report error to calling function and free allocated memory.*/
		  
		  /*free memory to avoid any memory leaks*/
		b_free(pBuffer);
		/*Assign pointer to NULL to avoid dangling pointers*/
		pBuffer = NULL;
		return NULL;
	}
	/*If all of the above was sucessful without error return a pointer of the created 
	buffer structure to the calling function*/
	return pBuffer;
}
/**********************************************************************************************************
Purpose:			Adds a character value to the buffer, if the buffer requires more space, 
					extra space will be allocated in memory if available
Author:				Jason Waid
History/Versions:	10/05/2019
Called Function:	b_isfull(), realloc()
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
												if value is Null no buffer structure is present
					symbol			-char	char within range of 0 and 255, char is added to the buffer
											
Return Value:		pBD				-*pBuffer that points to a valid Buffer structure

Algorithm:			if pBD is NUll then no buffer structure is present, reurn NULL
					to report error to calling function.
						Resets realocation flag before attempting to add character to buffer
						calls b_isfull() to determine if there is room in the buffer
							if room symbol is added to the next available buffer memory location
							determined by addc_offset.
							increments addc_offset for next attempt
							returns a pointer to the buffer struct.

						if no room in buffer mode will be checked
							if fixed mode no further action is required and function 
							returns NULL to report failure to the calling function

							if add mode capacity is incremented by the inc_factor
								if the new capacity is greater than SHRT max-1
								SHRT max-1 is considered the new buffer compacity

								if the new capaict yis less than zero null is returned
								to report error to calling function

							if multi mode capacity is checked
								if equal to maximum capacity (SHRT_MAX-1) null is returned
								to report error to calling function

								otherwise the following formulae is used to calculate the 
								new capacity

									available space = maximum buffer capacity - current capacity;
									new increment = available space * inc_factor / 100;
									new capacity = current capacity + new increment;

									if the result of the calulation is greater than the maximim 
									allowed buffer capacity, and the current capacity is smaller than the max,
									the maximim size is assigned to the buffer capacity

							if the mode is not recognized null is returned to calling fucntion to report error


						if the above is sucessful
							the buffer is realocated to a new memory size
							if unsucesssful null is returned to the calling function to report the error

							if the memory address of the buffer has changed, the realocation flag (r_flag) is
							updated accordingly to reflect this
							The symbol is added to the buffer using addc_offset
							addc_offset is incremented for the next add opperation
							the capacity data member of the buffer is updated to reflect the changes

							a pointer to the buffer structure is returned to the calling function
**********************************************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	char* temp_char_array_p;/*temporary pointer used to store new buffer memory address*/
	short new_capacity;/*hold new capacity of the buffer*/
	short available_space;/*holds available space in buffer*/
	short new_increment;/*holds the value of the new increment*/
	
	if (pBD == NULL) {
		return NULL;
	}

	/*Resets the r_flag used to track buffer memory address changes*/
	pBD->flags = pBD->flags & RESET_R_FLAG;

	/*Creates temp pointer for use in this method*/
	temp_char_array_p = pBD->cb_head;

	new_capacity = 0;

	/*Checks if the buffer is full, if there is space the character will be added*/
	if (b_isfull(pBD) == 0) {

		/*Add symbol to character array*/
		pBD->cb_head[pBD->addc_offset] = symbol;
		
		/*increment addc_offset*/
		pBD->addc_offset++;
		return pBD;
	}
	else {/*If the character buffer is full the following code will attempt to resize the character array*/

		/*If the operational mode is FIXED no resizing will happen*/
		if (pBD->mode == FIXED_MODE_NUM) {
			/*NULL will return to the calling function to report an error*/
			return NULL;
		}
		/*If the operation mode is Additive (1) the follow logic will be applied*/
		else if (pBD->mode == ADD_MODE_NUM) {

			/*New capacity is calculated by adding the increment factor to the current capacity*/
			new_capacity = pBD->capacity + (unsigned char) pBD->inc_factor;

			/*If the result of the above calculation is negative or larger than the MAXIMUM ALLOWED POSSITIVE VALUE (SHRT_MAX-1)
			the function will be allowed to continue*/
			
			/*If the above calculation exceeds the requirements the newCapacity will change to the capacity 
			to the MAXIMUM ALLOWED POSSITIVE VALUE*/
			if (new_capacity > MAXIMUM_BUFFER_CAPACITY) {
				new_capacity = MAXIMUM_BUFFER_CAPACITY;
				
			}
			/*if the result is negative the function will return NULL to the calling function to report the error*/
			else if (new_capacity < 0) {
				return NULL;
			}


		}
		/*If the opperational mode is Multiplicative the following logic will apply*/
		else if (pBD->mode == MULTI_MODE_NUM) {

			/*If the current capacity is already equal to the MAXIMUM ALLOWED POSSITIVE VALUE (SHRT_MAX-1) it cannot be
			incremented anymore, return NULL to the calling method to report the error*/
			if (pBD->capacity == MAXIMUM_BUFFER_CAPACITY) {
				return NULL;
			}

			/*The following formulae is used to calculate the new capacity in Multiplicative mode
					available space = maximum buffer capacity - current capacity;
					new increment = available space * inc_factor / 100;
					new capacity = current capacity + new increment;
			*/

			available_space = MAXIMUM_BUFFER_CAPACITY - pBD->capacity;
			/*Corrected Typo for inc_factor parameter Oct 9th 2019*/
			new_increment = available_space * (unsigned char) pBD->inc_factor / 100;
			/*If new increment is 0 we will set it back to 1 to avoid any buffer overflows*/
			if (new_increment == 0) {
				new_increment = 1;
			}
			new_capacity = pBD->capacity + new_increment;

			/*If the above calculation is more than  the MAXIMUM ALLOWED POSSITIVE VALUE (SHRT_MAX - 1)
			and the current capaicty is still smallaer than MAXIMUM ALLOWED POSSITIVE VALUE
			*/
			if ((MAXIMUM_BUFFER_CAPACITY > pBD->capacity) && (new_capacity > MAXIMUM_BUFFER_CAPACITY)) {

				new_capacity = MAXIMUM_BUFFER_CAPACITY;
			}
				
		}
		/*if the opperational mode is invalid return NULL to the calling function to report the error*/
		else {
			return NULL;
		}
	}

	/*The following code executes when the capacity increment was sucessful in Additive aor Multiplicative mode*/
	if (pBD->mode == ADD_MODE_NUM || pBD->mode == MULTI_MODE_NUM) {
		
		temp_char_array_p = pBD->cb_head;/*stores mem location of head before realocation*/
		temp_char_array_p = (char*) realloc(temp_char_array_p, sizeof(char)* new_capacity);/*attempts to reallocate memory using the new capacity*/

		/*If memory realocation failed reset cb_head to old memory location and return null to report error to 
		the calling function*/
		if (temp_char_array_p == NULL) {
			return NULL;
		}

		/*If the memory of the character array has has been reallocated, update the realocation flag (r_flag)
		and reassign the cb_head pointer to the new memory address*/
		if (pBD->cb_head - temp_char_array_p != 0) {
			pBD->flags = pBD->flags | SET_R_FLAG;
			pBD->cb_head = temp_char_array_p;
			temp_char_array_p = NULL; /*this is done to avoid dangling pointers*/
		}
		/*Appends the character symbol to thebuffer*/
		pBD->cb_head[pBD->addc_offset] = symbol;
		/*Increment addc_offset for the next time we add a character*/
		pBD->addc_offset++;
		/*store the new capacity in the structure*/
		pBD->capacity = new_capacity;
	}
		return pBD;
}
/**********************************************************************************************************
Purpose:			Clears the buffer data members, resets them to their default values
Author:				Jason Waid
History/Versions:	10/05/2019	
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		0				-0 is returned upon sucessful clear opperation
					RT_FAIL_1		-const is returned when a null pointer is passed into the function to report
									the error to the calling function

Algorithm:							The method simply resets the following Buffer data members to zero or their default values
									add_offset
									capacity
									getc_offset
									mode
									flags
**********************************************************************************************************/
int b_clear(Buffer* const pBD)
{

	/*If the pointer passed is NULL return fail indicator to the calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*reset everything in the struct, reinitialize it as it when when the memory was first allocated*/
	/*except for the memory allocation*/

	pBD->addc_offset = 0;
	/*pBD->capacity = 0;*/
	pBD->getc_offset = 0;
	pBD->inc_factor = 0;
	pBD->markc_offset = 0;
	pBD->mode = 0;
	pBD->flags = pBD->flags & DEFAULT_FLAGS;

	return 0;
}
/**********************************************************************************************************
Purpose:			Frees the Buffer structure and the character buffer if it exists
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									

Return Value:		No return value

Algorithm:			Simply free's the buffer

					if cb_head (the character buffer) isn't NULL it will free it as well
					Free the memory allocated ot the buffer structure
**********************************************************************************************************/
void b_free(Buffer* const pBD)
{
	
	/*free the memory allocated for the character buffer if it exists*/
	if (pBD->cb_head != NULL) {
		free(pBD->cb_head);
	}
	/*Free the Buffer structure*/
	free(pBD);
}
/**********************************************************************************************************
Purpose:			Detemrines if the character buffer is full
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		-1				-Returns this if the buffer is considered to be full

					-0				-Returns this if the buffer is not full

					-RT_FAIL_1		-const returns this if the passed pointer to the buffer structure is NULL
										this is done to report the error to the calling function

Algorithm:			If the passed pointer to the buffer structure is NULL, RT_FAIL_1 will be returned to the calling method
					to report the error

					if the capacity is equal to addc_offset, this means there is no more space in the buffer
					1 i returned to the calling method to report the buffer status

					else returns 0 to report available space in the buffer ot the calling function
**********************************************************************************************************/
int b_isfull(Buffer* const pBD)
{
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	/*if the capacity is equal to addc_offset the buffer is full*/
	if (pBD->capacity == pBD->addc_offset) {
		return 1;
	}else
		return 0;
}
/**********************************************************************************************************
Purpose:			returns the current value of addc_offset which determines the the current size of used buffer bits
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		-RT_FAIL_1		-const returns fail indicator -1 to report the error to the calling function,
									if the passed pointer is null

					-addc_offset	-short this is returned to determine the current limit fo the buffer

Algorithm:			If the passed pointer to the buffer structure is NULL, the above fail indicator is returned
					to rpeort to error to the calling function

					otherwise the capaict yis returned to the calling function
**********************************************************************************************************/
short b_limit(Buffer* const pBD)
{
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {	
		return RT_FAIL_1;
	}
	/*Returns current limit of the character buffer
	ammount of space measured in chars currently being used*/
	return pBD->addc_offset;
}
/**********************************************************************************************************
Purpose:			Returns the current capacity of the buffer
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		-RT_FAIL_1		-const returns fail indicator -1 to report the error to the calling function,
									if the passed pointer is null

					-capacity		-short this is returned to determine the current capacity fo the buffer

Algorithm:			If the passed pointer to the buffer structure is NULL, the above fail indicator is returned
					to rpeort to error to the calling function

					otherwise the capaicty is returned to the calling function
**********************************************************************************************************/
short b_capacity(Buffer* const pBD)
{
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	/*return current capacity of the character buffer*/
	return pBD->capacity;
}
/**********************************************************************************************************
Purpose:			Returns the value of markc_offset to the calling function
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

					-mark			-short	contains the new mark for the buffer

Return Value:		-RT_FAIL_1		-const returns fail indicator -1 to report the error to the calling function,
									if the passed pointer is null
									-also returned when mark is less than 0 or greater than addc_offset

					-markc_offset	-this is returned to determine the markc_offset was sucessfully updated

Algorithm:			If the passed pointer to the buffer structure is NULL, the above fail indicator is returned
					to report to error to the calling function

					If mark is within the range of 0 and addc_offset inclusive the markc_offset is updated to mark
					the new markc_offset is then returned to report the sucess to the calling function

					If the mark is less than zero or greater than addc_offset is outside of the logical boundaries of the buffer
					therefore RT_FAIL_1 is return to report the error to the calling function
**********************************************************************************************************/
short b_mark(pBuffer const pBD, short mark)
{
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	/*if the mark is within the current limit of the buffer ( 0 to addc_offset inclusive) assign mark and 
	return mark to the calling function*/
	else if (mark >= 0 && mark <= pBD->addc_offset) {
		pBD->markc_offset = mark;
		return pBD->markc_offset;
	}

	return RT_FAIL_1;
}
/**********************************************************************************************************
Purpose:
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		-RT_FAIL_1		-const returns fail indicator -1 to report the error to the calling function,
									if the passed pointer is null
									-also returned if the buffer mode is invalid

					-mode			-char returns the current value of the buffer mode

Algorithm:			If the passed pointer to the buffer structure is NULL, the above fail indicator is returned
					to report to error to the calling function

					If mark is mode is valid it is then returned to report the sucess to the calling function

					If mark is mode is invalid RT_FAIL_1 is returned to report error ot calling function
**********************************************************************************************************/
int b_mode(Buffer* const pBD)
{
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	/*return the value of the mode to the calling function if the mode is valid*/
	else if (pBD->mode == FIXED_MODE_NUM || pBD->mode == MULTI_MODE_NUM || pBD->mode == ADD_MODE_NUM) {
		return pBD->mode;
	}
	return RT_FAIL_1;
}
/**********************************************************************************************************
Purpose:			Returns the increment factor of the buffer
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		-0x100			-Hex value	if the passed pointer to the buffer structure is NULL
									returned to calling function to report error

					-inc_factor		-unsigned char	the current inc_factor of the buffer

Algorithm:			if the passed pointer to the buffer structure is NULL the hex value 0x100 is returned
						to the calling function to report the error

					otherwise the inc_factor of the buffer is returned to the calling function, cast to 
					unsigned char to ensure the type remains the same.
**********************************************************************************************************/
size_t b_incfactor(Buffer* const pBD)
{

	/*if the passed pointer is NULL return HEX value 0x100 to calling function*/
	if (pBD == NULL) {
		return 0x100;
	}

	/*return non-negative value of inc_factor*/
	return (unsigned char) pBD->inc_factor;

}
/**********************************************************************************************************
Purpose:			adds the characters to the buffer by calling b_addc when a character is recognized
Author:				Jason Waid
History/Versions:	10/05/2019
Called Function:	feof(), fgetc(), b_addc(), ungetc()
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

					fi				-FILE const	  points to a valid file pointer

Return Value:		-RT_FAIL_1		-const returns fail indicator -1 to report the error to the calling function,
									if either of the passed pointers are null

					-LOAD_FAIL		-const returns fail indicator -2 to report the error to the calling function,
									if the passed pointer is null

					-num_chars_added	-int the number of characters added to the buffer

Algorithm:			if either of the passed pointers are NULL return RT_FAIL_1 to report an error to the calling function

						Call feof() to determine end of file while traversing the file buffer using a while loop
							using fgetc() take the current character stored in the file buffer and apply it to character variable
							if the charcater stored is not the EOF charcater attempt to add the character to the buffer
								if as a result of adding the charcter Null is returned, the opperation failed.
								Use ungetc() to place the charcater back into the file buffer and return load fail

								otherwise the opperation worked and the character was added, so increment num_chars_added.

						Once EOF is detected exit the loop and return the number of characters added to the buffer 
						to the claling function
**********************************************************************************************************/
int b_load(FILE* const fi, Buffer* const pBD)
{

	char character; /*hold the character from the file*/
	int num_chars_added = 0; /*Counts the number of characters that are added to the buffer*/
	
	/*if either the passed pointers are NULL return Fail indicator to report error to calling function*/
	if (fi == NULL || pBD == NULL) {
		return RT_FAIL_1;
	}

	/*Loops that will examine every character in the file up till the EOF character*/
	while (!feof(fi)) {
		/*Take a character from the file buffer and place it into character for examination*/
		character = (char)fgetc(fi);
		
		/*if the character is not equal to the End Of File character, continue*/
		if (character != EOF) {
			/*Add's the character to the buffer*/
			if (b_addc(pBD, (char)character) == NULL) {
				ungetc(character, fi);
				return LOAD_FAIL;
			}
			else
				num_chars_added++;
		}
	}
	
	/*return number of characters added to buffer to the calling function*/
	return num_chars_added;
}
/**********************************************************************************************************
Purpose:			Determines if the buffer is empty
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		-RT_FAIL_1		-const returns fail indicator -1 to report the error to the calling function,
									if  the passed pointer is null

					-1				- returns 1 to the calling function if the buffer is empty

					-0				- returns 0 if the buffe ris not empty

Algorithm:			if the passed pointer to the buffer structure is NULLL return RT_FAIL_1 to the calling function
					to report the error

					if addc_offset is equal to zero, this means the next charcater to be added to the buffer is at 
					the start of the buffer, therefore it is empty. return 1 to the calling function to report
					that the buffer is empty

					otherwise return 0 to report that the buffer is not empty to the calling function
**********************************************************************************************************/
int b_isempty(Buffer* const pBD)
{
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*if addc_offset is equal to zero the buffer is empty*/
	if (pBD->addc_offset == 0) {
		return 1;
	}
	else
		return 0;

	
	
}
/**********************************************************************************************************
Purpose:			gets the character located at the getc_offset
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		-RT_FAIL_2		-const returns fail indicator -2 to report the error to the calling function,
									if  the passed pointer is null

					-character		-char	the character located at getc_offset

					-0				reports end of buffer

Algorithm:			if the passed pointer to the buffer structure is NULL return RT_FAIL_2 const to report
					the error to the calling function

					if getc_offset is equal to addc_offset set the end of buffer flag accordingly
						return 0 to report end of buffer ot the calling function

					else reset the end of buffer flag and store the character located at thegetc_offset
					in the character variable.

					increment getc_offset for the next get opperation

					return the character that is store in the character variable.
**********************************************************************************************************/
char b_getc(Buffer* const pBD)
{
	char character;

	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_2;
	}

	/*if getc_offset and addc_offset are equal we are at the end of the buffer*/
	if (pBD->getc_offset == pBD->addc_offset) {
		/*Updates End Of Buffer flag*/
		pBD->flags = pBD->flags | SET_EOB;
		/*returns 0 to the calling function*/
		return 0;
	}else{
		/*Resets the End Of Buffer flag as we are not at the end of the buffer*/
		pBD->flags = pBD->flags & RESET_EOB;
	}

	character = pBD->cb_head[pBD->getc_offset];

	/*increments getc-offset for when we get our next character*/
	pBD->getc_offset++;
	/*return character located at getc_offset - 1 before we incremented getc_offset*/
	return character;
	
}
/**********************************************************************************************************
Purpose:			checks the end of buffer flag
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		-RT_FAIL_1		-const returns fail indicator -1 to report the error to the calling function,
									if  the passed pointer is null

					-flags			-unsigned short uses the and opperator in conjustion with the CHECK_EOB const to
									return the second last bit of the falgs field used to determine the end of buffer flag

Algorithm:						if the passed pointer to the buffer structure is null return RE_FAIL_1 const to the calling function
								to report the error

								otherwise return  the value of the second to last bit of the flags field using the CHECK_EOB const
								and the and opperation to report the value of the end of buffer flag to the calling function
**********************************************************************************************************/
int b_eob(Buffer* const pBD)
{
	/*if the passed pointer is NULL return NULL to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	/*return the value of the End OF Buffer flaf to the calling function*/
	return pBD->flags & CHECK_EOB;
}
/**********************************************************************************************************
Purpose:			Prints the contents of the buffer
Author:				Jason Waid
History/Versions:	10/05/2019
Called Function:	b_getc(), b_eob(), printf()
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

					nl				-char nl represents a new line character

Return Value:		-RT_FAIL_1		-const returns fail indicator -1 to report the error to the calling function,
									if  the passed pointer is null

					-num_characters	-int the number of characters printed

Algorithm:			if the passed pointer to the buffer structure is NULL return RT_FAIL_1 to the calling function
					to report the error

					reset getc_offset to zero, as this determines the current character we'll be printing
					do the following while we are not at the end of the buffer
						using b_getc() take the current character and copy it to buffer_character 
						(temp variable for storing the character) using b_eob determine if we have reached 
						the end of buffer
							if we are we must break out of the while loop
						print the charcter stored in buffer_character using printf()
						increment the number of characters printed (num_characters)

					after the while loop is done, if nl is not equal to zero print a new line character 
					using printf()

					Return the number of characters printed to the calling function
**********************************************************************************************************/
int b_print(Buffer* const pBD, char nl)
{
	int num_characters = 0;
	char buffer_character;
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	pBD->getc_offset = 0;

	/*function used for diagnostic purposes only*/
	/*print chatacter by character the contents of the buffer*/
	do{
		
		buffer_character = b_getc(pBD);
		if (b_eob(pBD) == 2) {
			break;
		}
		
		printf("%c", buffer_character);
		num_characters++;

	} while (b_eob(pBD) != 2);


	if (nl != 0) {
		printf("\n");
	}

	/*return num of characters printed*/
	return num_characters;
}
/**********************************************************************************************************
Purpose:			Changes the size of the character buffer on demand
Author:				Jason Waid
History/Versions:	10/05/2019
Called Functions:	realloc(), sizeof()
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

					symbol			-char	symbol to be added to the end of the buffer

Return Value:		NULL			-	 reports error the the calling function

					pBD				-pBuffer const	points to a valid buffer structure

Algorithm:			if the passed pointer ot the buffer structure is NULL return NULL to report
					the error to the calling function

					assign a temp pointer to the character buffer

					assign the new capacity to the curretn addc_offset plus 1
					attempt to reallocate the memory using realloc()
					if the pointer returned by realloc() is NULL return NULL to the calling function
					to report the error

					
					if the memory address of cb_head and the temp pointer are different set the memory
					realocation flag appropriately sinc ethe memory address changed
					
					take the new pointer and assign it to the character buffer
					set the temp pointer to NULL
					update the capacity to the newly calculated capacity (new capacity)
					add the passed symbol to the character buffer determined by the values of addc_offset
					increment addc_offset

					return a pointer to the buffer structures
**********************************************************************************************************/
Buffer* b_compact(Buffer* const pBD, char symbol)
{
	char* temp_char_array_p;/*temp pointer stores memory location to char array after realocation*/
	short new_capacity = 0;/*hold the new capacity used for resizing the char array*/

	/*if the passed pointer is NULL return NULL to report error to calling function*/
	if (pBD == NULL) {
		return NULL;
	}

	temp_char_array_p = pBD->cb_head; /*assigns the temp char array to the actual char array*/
	
	new_capacity = pBD->addc_offset + 1; /*calculation of new capacity, current capacity plus one*/

	/*attempts to reallocate memory for buffer*/
	temp_char_array_p = (char*) realloc(temp_char_array_p, sizeof(char) * new_capacity);

	/*if reallocation fails, return NULL to the calling function to report error*/
	if (temp_char_array_p == NULL) {
		return NULL;
	}
	/*Sets realocate flag and reasigns cb_head to new memory alloaction if allocated memory changed*/
	else {
		
		if (pBD->cb_head - temp_char_array_p == 0) {
			temp_char_array_p = NULL; /*this is done to avoid any dangling pointers*/
		}
		else {
			pBD->cb_head = temp_char_array_p;
			pBD->flags = pBD->flags | SET_R_FLAG;
		}
	}

	/*updates capacity upon success memory reallocation*/
	pBD->capacity = new_capacity;
	/*adds symbol to array*/
	pBD->cb_head[pBD->addc_offset] = symbol;
	/*increments addc offset for next character*/
	pBD->addc_offset++;
	
	/*return buffer pointer to calling function*/
	return pBD;
}
/**********************************************************************************************************
Purpose:			returns the value of the memory realocation flag
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		FT_FAIL_1		-const returned to the calling function to report error
					flags			-unsigned short	returned allong with the and opperator to the CHECK_R_FLAG const
									to determine the value fo the realocation flag

Algorithm:			if the passed pointer is NULL return RT_FAIL_1 to report the error to the calling
					function

					otherwise return the right most bit of the flags field by using the and opperator
					in conjuction with the CHECK_R_FLAG const
**********************************************************************************************************/
char b_rflag(Buffer* const pBD)
{
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	/*use bitwise opperations to return value of r_flag bit to calling function*/
	return (char) pBD->flags | CHECK_R_FLAG;
}
/**********************************************************************************************************
Purpose:			Decrements the getc_offset and returns the addc_offset
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		FT_FAIL_1		-const returned to the calling function to report error if passed pointer
									is NULL or the getc_offset cannot be decremented

					addc_offset		-short the next location a charcater will be added to the buffer
					

Algorithm:			if the passed pointer is NULL return RT_FAIL_1 to report the error to the calling
					function

					if the getc_offset is already zero return the same fail indicator to report the error

					otherwise decrement the getc_offset by 1 and return the addc_offset to the calling
					function
**********************************************************************************************************/
short b_retract(Buffer* const pBD)
{
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}
	/*if the getc_offset is already 0 return Fail indicator to report error to calling function*/
	else if (pBD->getc_offset == 0) {
		return RT_FAIL_1;
	}

	pBD->getc_offset--;

	/*return addc_offset to the calling function*/
	return pBD->addc_offset;
}
/**********************************************************************************************************
Purpose:			sets the getc_offset to the marc_offset
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		FT_FAIL_1		-const returned to the calling function to report error if passed pointer

					getc_offset		-short	the offset from the buffer for the next character to be taken

Algorithm:			if the passed pointer is NULL return RT_FAIL_1 to report the error to the calling
					function

					set getc_offset to markc_offset

					return the value of getc_offset to the calling function
**********************************************************************************************************/
short b_reset(Buffer* const pBD)
{
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	
	pBD->getc_offset = pBD->markc_offset;
	/*return new getc_offset to the calling function*/
	return pBD->getc_offset;
}
/**********************************************************************************************************
Purpose:			return the value of the getc_offset to the calling function
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		FT_FAIL_1		-const returned to the calling function to report error if passed pointer

					getc_offset		-short the value of offset for the next character to be taken from the buffer

Algorithm:		if the passed pointer is NULL return RT_FAIL_1 to report the error to the calling
				function

				return the value of the getc_offset to the calling function
**********************************************************************************************************/
short b_getcoffset(Buffer* const pBD)
{
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	/*return getc_offset to the calling function*/
	return pBD->getc_offset;
}
/**********************************************************************************************************
Purpose:			resets getc_offset and markc_offet to zero
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		FT_FAIL_1		-const returned to the calling function to report error if passed pointer
					0				- return when the opperation was sucessful

Algorithm:			if the passed pointer is NULL return RT_FAIL_1 to report the error to the calling
					function

					set getc_offset and markc_offset to zero
					return 0 to the calling function upon success
**********************************************************************************************************/
int b_rewind(Buffer* const pBD)
{
	/*if the passed pointer is NULL return Fail indicator to report error to calling function*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	pBD->getc_offset = 0;
	pBD->markc_offset = 0;

	/*return 0 to the calling function to report a successful opperation*/
	return 0;
}
/**********************************************************************************************************
Purpose:			returns the address
Author:				Jason Waid
History/Versions:	10/05/2019
Parameters:			pBD				-pBuffer const	points to a valid buffer structure in memory
									if value is Null no buffer structure is present

Return Value:		FT_FAIL_1		-const returned to the calling function to report error if passed pointer

					pointer to the character located in the cb_head at markc_offset - *char

Algorithm:			if the passed pointer is NULL return RT_FAIL_1 to report the error to the calling
					function

					return a pointer to the character located in the character buffer at markc_offset
**********************************************************************************************************/
char* b_location(Buffer* const pBD)
{

	/*if the passed pointer is NULL return NULL to report error to calling function*/
	if (pBD == NULL) {
		return NULL;
	}
	/*return location of the character buffer*/
	return &pBD->cb_head[pBD->markc_offset];
}
