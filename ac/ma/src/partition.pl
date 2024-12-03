/* FILE:      partition.pl
   PURPOSE:   Create partitions of note sequences
   REQUIRES:  SWI Prolog (https://www.swi-prolog.org)
   AUTHOR:    Mick de Neeve <mick@live.nl>
   INSTITUTE: University of Amsterdam
   DATE:      December 3, 2024
*/

:- ensure_loaded([subs]).

/* partition(+Notes/Degrees, -Partitions)
      Given lists Notes/Degrees, i.e. notes with their degree
      substitutions, find all partitions; returning them in lists
      of the form NotePartitions/DegreePartitions; with smallest
      last and excluding the original ('self partition').
      
*/
partition(Notes/Subs, Partitions) :-
   findall(  NoteParts/SubParts,
             (  subparts(Subs,SubParts),
                noteparts(SubParts,Notes,NoteParts)
             ), Parts),
   reverse(Parts, [_|Partitions]).

/* partition(+Degrees, -Partitions)
      Given a lists of Degrees, find all partitions; returning
      them in lists of the form DegreePartitions; with smallest
      last and excluding the original ('self partition').
      
*/
partition(Notes, Harms, Partitions) :-
   findall(SubParts, subparts(Subs,SubParts), Parts),
   reverse(Parts, [_|Partitions]).

/* subparts(+DegreesList, -DegreesPartitions)
      List of lists DegreesPartitions requires partitions end
      stable and are at least 2 elements (for forces assigments).
*/
subparts([], []).
subparts([Sub|STail], [[Sub|NewSTail]|SubParts]) :-
   append(NewSTail, NewSub, STail),
   length(NewSTail, SLength),
   last(NewSTail, Final),
   SLength > 0,
   member(Final, [0, 4, 2]),
   subparts(NewSub, SubParts).

/* noteparts(+DegreePartitions, +Notes, -NotePartitions)
      NotePartitions is a partitioning of Notes such that it
      aligns with DegreePartitions.
*/
noteparts([], _, []).
noteparts([Part|Parts], Notes, [NotePart|NoteParts]) :-
   append(NotePart, NoteRest, Notes),
   length(Part, L),
   length(NotePart, L),
   noteparts(Parts, NoteRest, NoteParts), !.
