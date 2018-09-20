/* 
 * CS:APP Data Lab 
 * 
 * <Please put your name and userid here>
 *  KimJungwoo cs20140681
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */

#endif
/* Copyright (C) 1991-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
/* 
 * bitAnd - x&y using only ~ and | 
 *   Example: bitAnd(6, 5) = 4
 *   Legal ops: ~ |
 *   Max ops: 8
 *   Rating: 1
 */
int bitAnd(int x, int y)
{
  // Using De Morgan's Law
  // X AND Y =  NOT ( (NOT X)  OR (NOT Y) )
  // We can make NOT with bitwise operator ~
  return ~((~x) | (~y));
}
/* 
 * getByte - Extract byte n from word x
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: getByte(0x12345678,1) = 0x56
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int getByte(int x, int n)
{
  // If we want to MSB, we have to shift to right 3 Bytes (24 bits)
  // we make 24 with (3 <<3) because 24 = 11000 = ((11)<<3)
  // 2Bytes=16Bits is 16 = 10000 = (10)<<3 and 1Byte=8Bits is 8=1000 = (1)<<3
  // And we don't want to be affected by unwanted 1s from arithmetic shift,
  // so we use "& 255" to take only last 8bits
  return ((x >> ((n) << (3))) & 255);
}
/* 
 * logicalShift - shift x to the right by n, using a logical shift
 *   Can assume that 0 <= n <= 31
 *   Examples: logicalShift(0x87654321,4) = 0x0876  5432
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3 
 */
int logicalShift(int x, int n)
{
  // Basic right shift in C is arithmetic shift,
  // so we have to deal with unwanted 1s in left of result
  // There would be n wanted 1s or not.
  // So we will take only last (32-n) bits of result using bitwise & and LOGSHIFT

  int LOGSHIFT = ~((((!!n) << 31) >> (n)) << 1);

  return (x >> n) & LOGSHIFT;
}
/*
 * bitCount - returns count of number of 1's in word
 *   Examples: bitCount(5) = 2, bitCount(7) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */

int bitCount(int x)
{
  // We will deal with each byte first using DIVIDE AND CONQUER Algorithm
  // Make basis for dealing with each byte, it just recursively add 0x01
  int basis = 1 + (1 << 8) + (1 << 16) + (1 << 24);

  // Then we will calculate number of 1's in each byte
  // and save it in the last bit of each byte.

  int sum = (x & basis) + ((x >> 1) & basis) + ((x >> 2) & basis) + ((x >> 3) & basis) + ((x >> 4) & basis) + ((x >> 5) & basis) + ((x >> 6) & basis) + ((x >> 7) & basis);

  // Now we add all last bits of all bytes.
  // We have to concentrate on last 7 bits because there couldn't be more than 32 1's.
  return ((sum + (sum >> 8) + (sum >> 16) + (sum >> 24)) & 63);
}
/* 
 * bang - Compute !x without using !
 *   Examples: bang(3) = 0, bang(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int bang(int x)
{
  //We want to return 1 when x is 0, otherwise return 1.
  //For all x!=0, x!=-x, but x=-x for x=0
  //So I'll return 1 if x=-x, especially using signs of x and -x

  //Get the sign of x
  int sig = (x >> 31) & 1;

  // Since x+(~x)+1=0, -x=(~x)+1. 
  // I can get the sign of -x with ( ( ((~x) + 1) >> 31) & 1)
  // If signs of x and -x are different, bitwise operator ^ of them would be 1.
  // At that time, we have to return 0, I use ~ operator to make 1 to 0,
  // and use & operator to make return value 0.
  // And vice varsa

  return (~sig) & ( (~(sig ^ ( ( ((~x) + 1) >> 31) & 1) )) & 1);
}
/* 
 * tmin - return minimum two's complement integer 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmin(void)
{
  //I shoud return 0x80000000
  return (1 << 31);
}
/* 
 * fitsBits - return 1 if x can be represented as an 
 *  n-bit, two's complement integer.
 *   1 <= n <= 32
 *   Examples: fitsBits(5,3) = 0, fitsBits(-4,3) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */

int fitsBits(int x, int n)
{
  // If x is positive, it would look like 00000101110.....
  // We have to catch index of first 1 is smaller than n-1
  // We should think n-1 because one 0 in front of first 1 is also part of x
  // We can check it with (x>> (n-1)) is all 0 bits

  // If x is negative, it would look like 111101101....
  // We have to catch index of first 0 is smaller than n-1
  // We can check it with (x>> (n-1)) is all 1 bits

  // To use n-1, I use fact that n+(~n)+1=0

  int m; 
  m = n + n + (~n); //m=n-1

  return !(x >> (m)) | !(~((x >> (m))));
}

/* 
 * divpwr2 - Compute x/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   Examples: divpwr2(15,1) = 7, divpwr2(-33,4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int divpwr2(int x, int n)
{
  // x>>n is doing well with positive x
  // When x is negative, rounding toward zero is different.
  //So we have to add remainder if it existed

  int semi = x >> n;
  semi = semi + ((!!((semi << n) + (~x) + 1)) & ((x >> 31) & 1));
  return semi;
}

/* 
 * negate - return -x 
 *   Example: negate(1) = -1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int negate(int x)
{
  //Using fact x+(~x)+1=0
  return (~x) + 1;
}
/* 
 * isPositive - return 1 if x > 0, return 0 otherwise 
 *   Example: isPositive(-1) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 3
 */
int isPositive(int x)
{
  //Check first bit of x with ((x >> 31) & 1)
  // But we have to deal with zero
  // So I use (!!x)
  return (!!x) & (!((x >> 31) & 1));
  
}
/* 
 * isLessOrEqual - if x <= y  then return 1, else return 0 
 *   Example: isLessOrEqual(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLessOrEqual(int x, int y)
{
  // Just checking x-y<=0 isn't enough
  // We have to check positive/negative overflow
  // But overflow exist only whenever signs of two arguments are different
  // (!sa) is when x>=0 and y<0, always x>y
  // s is to deal with x<0 and y>=0
  // I also check whether x and y are same or not using !(x^y)
  int l = !(x ^ y);
  int xx = (x >> 31) & 1;
  int yy = (y >> 31) & 1;

  int z = x + (~(y)) + 1;
  int a = ((z >> 31) & 1);

  int sa = !((xx ^ yy) & xx);
  int s = !((xx ^ yy) & yy);

  return ((!sa) | (s & (l | (!(a ^ s)))));
}
/*
 * ilog2 - return floor(log base 2 of x), where x > 0
 *   Example: ilog2(16) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 90
 *   Rating: 4
 */
int ilog2(int x)
{
  // To find the first 1 is most important.
  int oh, gogo, total;

  // First, I'll put 1 to all bits after first 1
  // If there is 1, all bits after that bit should be 1
  x = x | (x >> 16);
  x = x | (x >> 8);
  x = x | (x >> 4);
  x = x | (x >> 2);
  x = x | (x >> 1);

  // Not just count the number of 1's
  // I use same algorithm in bitCount()
  oh = 1 + (1 << 8) + (1 << 16) + (1 << 24);
  gogo = (x & oh) + ((x >> 1) & oh) + ((x >> 2) & oh) + ((x >> 3) & oh) + ((x >> 4) & oh) + ((x >> 5) & oh) + ((x >> 6) & oh) + ((x >> 7) & oh);
  total = ((gogo + (gogo >> 8) + (gogo >> 16) + (gogo >> 24)) & 63);

  //Do not forget to -1, using fact x+(~x)+1=0
  return total + total + (~total);
}

/* 
 * float_neg - Return bit-level equivalent of expression -f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */

unsigned float_neg(unsigned uf)
{
  //We have to change only signal bit (first one bit)
  if ((((uf >> 23) & 255) != 255) | (!(uf & 0x7fffff)))
  {
    unsigned tempuf;
    // Only changing first bit of uf
    tempuf = (0x7fffffff & uf) + (0x80000000 & (~uf));
    return tempuf;
  }

  //To deal with NAN
  return uf;
}
/* 
 * float_i2f - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */

unsigned float_i2f(int x)
{
  int sign, where;
  unsigned y, z, sticky, where24, frac, ans;
  //First we deal with zero
  if (!x)
  {
    return x;
  }
  where = 31;
  sign = 0;
  y = x;

  //If x is negative, just keep in mind its signal and
  //calculate float of -x. Then merge it with its orginal signal
  if (x < 0)
  {
    sign = 1;

    y = -x;
  }

  //I try to find the index of first bit with 1
  z = y;
  while (!(z & 0x80000000))
  {
    where--;
    z = z << 1;
  }
  sticky = 0;
  where24 = where - 24;

  if (where > 23)
  {
    //When where >23, we have to think about remainder(sticky)
    //because frac part of float is only 23 bits.

    // Let's say a,b are 23 and 24th bit, and k is rest bits after b
    // If a=1,b=1(case 3) -> rounding up
    // If b=0 (case 0 and 2 which are in default) -> stay ame
    // If a=0,b=1(case 1) -> It depends on whether k is nonzero
    // I got above idea from KLMS Q&A which are written by Klvchev, Kalomidin
  
    unsigned atob = (y >> where24) & 3;
    switch (atob)
    {
    case 1:
    {
      if (where > 24)
      {
        int whereindex;
        for (whereindex = 0; whereindex < where24; whereindex++)
        {
          if ((y >> whereindex) & 1)
          {
            sticky = 1;
            break;
          }
        }
      }
      break;
    }
    case 3:
    {

      sticky = 1;
      break;
    }

    default:
      break;
    }
    //Then, we have to add sticky to first 23 bits of frac
    frac = (((y >> (where - 23)) & (0x7fffff)) + sticky);
  }
  else
  {
    // If where<=23, we don't have to think about remainder(sticky).
    // Buy don't forget to shift it again to make first bit of fract should be
    // just after exp part
    frac = ((y & ((1 << where) - 1)) << (23 - where));
  }

  ans = (sign << 31) + ((127 + where) << 23) + frac;

  return ans;
}

/* 
 * float_twice - Return bit-level equivalent of expression 2*f for
 *   floating point argument f.
 *   Both the argume*nt and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_twice(unsigned uf)
{
  
  unsigned expp, frac;
  // To deal with zero and 0x80000000
  if ((uf & 0x7fffffff) == 0)
  {
    return uf;
  }
  //Get exp part of float
  expp = (uf >> 23) & 0xff;
  //Get frac part of float
  frac = uf & 0x7fffff;

  // To deal with special value
  if (expp == 0xff)
  {
    return uf;
  }
  // To deal with denormalized value
  // Only doubling frac part
  if (expp == 0)
  {
    return uf + frac;
  }
  // To deal with normalized value
  // Only add 1 to exp part
  return uf + (1 << 23);
}
