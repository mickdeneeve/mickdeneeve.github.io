/* FILE:      lists.pl
   PURPOSE:   Provide utility predicates for lists
   REQUIRES:  SWI Prolog (https://www.swi-prolog.org)
   AUTHOR:    Mick de Neeve <mick@live.nl>
   INSTITUTE: University of Amsterdam
   DATE:      December 3, 2024
*/

/* index(+List, +StartIndex, -IndexedList)
      IndexedList is List with its elements prefixed in the form
      index:element, starting from StartIndex.
*/
index([], _, []).
index([H|T], I, [I:H|NT]) :-
   J is I + 1,
   index(T, J, NT).

/* select(+IndexedPart, +IndexedList, -IndexedSelected)
      IndexedList is full list; IndexedPart is subset of its indeces
      but may have different elements, and IndexSelected gets the
      elements of IndexedList with matching indeces in IndexParts.
*/
select([], _, []).
select([I:_|PTail], IList, [I:Elt|STail]) :-
   member(I:Elt, IList), !,
   select(PTail, IList, STail).
select([_|PTail], IList, STail) :-
   select(PTail, IList, STail).

/* replace(+Index, +Element, +List, -ReplacedList)
      ReplacedList is List with Element substituted for whatever was
      originally at Index (0-based); using the auxiliary replace/5
      that includes a starting index as its second argument (so
      replace/5 replaces only from this index).
*/
replace(Index, Elt, List, Replaced) :-
   length(List, Len),
   Index < Len,
   replace(Index, 0, Elt, List, Replaced), !.   
replace(Index, Index, Elt, [_|List], [Elt|List]).
replace(Index, Lower, Elt, [Head|List], [Head|Replaced]) :-
   Next is Lower + 1,
   replace(Index, Next, Elt, List, Replaced).

/* list(+List)
      Print list elements to standard output.
*/
list([]).
list([Head|Tail]) :-
   writeln(Head),
   list(Tail).
