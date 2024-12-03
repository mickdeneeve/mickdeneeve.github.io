/* FILE:      maths.pl
   PURPOSE:   Provide utility predicates for maths
   REQUIRES:  SWI Prolog (https://www.swi-prolog.org)
   AUTHOR:    Mick de Neeve <mick@live.nl>
   INSTITUTE: University of Amsterdam
   DATE:      December 3, 2024
*/

/* round(+Float, +Decimals, -Rounded)
      Round Float to a given number of decimal places.
*/
round(Float, Dec, Rounded) :-
   Num is Float * 10^Dec,
   round(Num, Int),
   Rounded is Int / 10^Dec.
