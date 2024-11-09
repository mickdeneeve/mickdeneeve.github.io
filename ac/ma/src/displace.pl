/* FILE:      displace.pl
   PURPOSE:   Find stepwise successions of notes in a sequence
   REQUIRES:  SWI Prolog (https://www.swi-prolog.org)
   AUTHOR:    Mick de Neeve <mick@live.nl>
   INSTITUTE: University of Amsterdam
   DATE:      December 1, 2024
*/

/* displace(+Notes, -Displaced)
      Displaced is a two-way non-contiguous but sequential
      partitioning of Notes that has a stepwise partition.
*/
displace(Notes, Displaced) :-
   divide(Notes, Displaced),
   member(Steps, Displaced),
   stepwise(Steps),
   length(Displaced, 2).

% stackoverflow.com/questions/30142740

/* divide(+Notes, -Divided)
      Divided is a non-contiguous but sequential list of lists
      partitioning of Notes. Auxiliary divide/3 makes subdivisions
      among two lists first.
*/
divide([], []).
divide([Note|Notes], [[Note|DNotes]|Divided]) :-
   divide(Notes, DNotes, Rest),
   length(DNotes, L),
   L >= 1,
   divide(Rest, Divided).
divide([], [], []).
divide([Note|Notes], [Note|DNotes], Divided) :-
   divide(Notes, DNotes, Divided).
divide([Note|Notes], DNotes, [Note|Divided]) :-
   divide(Notes, DNotes, Divided).

/* stepwise(+Notes)
      Succeeds if Notes has no successor transitions greater than 1.
*/
stepwise([N1, N2]) :-
   stepwise(N1, N2), !.
stepwise([N1,N2|Notes]) :-
   stepwise(N1, N2),
   stepwise([N2|Notes]).
stepwise(N1, N2) :-
   S is abs(N1-N2),
   S =< 1.
