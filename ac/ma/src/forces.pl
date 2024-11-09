/* FILE:      forces.pl
   PURPOSE:   Compute Larson's forces for note sequences
   REQUIRES:  SWI Prolog (https://www.swi-prolog.org)
   AUTHOR:    Mick de Neeve <mick@live.nl>
   INSTITUTE: University of Amsterdam
   DATE:      December 1, 2024
*/

:- ensure_loaded([maths]).

/* gravitational(+Notes)
      Succeeds if the final note descends and is lower than the
      first. This is a crude version of Steve Larson's notion, e.g.
      no ceiling is specified beyond the partition's first note.
      Auxiliary gravitational/3 applies this first-note ceiling.
*/
gravitational([First|Notes]) :-
   reverse([First|Notes], [Final,Penultimate|_]),
   gravitational(First, Penultimate, Final), !.
gravitational(Note, Note, Final) :-
   Note > Final.
gravitational(First, Penultimate, Final) :-
   First >= Final,
   Penultimate > Final.

/* kmagnetic(+Scale, -Kmagnetism)
      Gives the Ionic major or Aeloic minor scale vectors from
      Krumhansl's Cognitive Foundations of Musical Pitch (1990).
*/
kmagnetic(imaj, [6.35,3.48,4.38,4.09,5.19,3.66,2.88]).
kmagnetic(amin, [6.33,3.52,5.38,3.53,4.75,3.98,3.34]).

/* kmagnetic(+Scale, +Partition, -Kvalue)
      Gives a Krumhansl magnetic value determined between the
      sequence's penultimate and last notes, which is set to be
      last/penultimate so it is asymmetric unlike Larson's notion.
*/
kmagnetic(Scale, Notes, K) :-
   reverse(Notes, [Final,Penultimate|_]),
   kmagnetic(Scale, Penultimate, Final, K).
kmagnetic(Scale, Penultimate, Final, K) :-
   kmagnetic(Scale, Values),
   PRole is Penultimate mod 7,
   FRole is Final mod 7,
   nth0(PRole, Values, PValue),
   nth0(FRole, Values, FValue),
   KF is FValue / PValue,
   round(KF, 2, K).

/* inertic(+Notes, -Type)
      Determines inertia for a note sequence, following Larson,
      (if present); which is defined as 'following a pattern',
      and specified here as either repeating its notes ('still'),
      ascending ('up'), descending ('down'), or 'alternating'.
*/
inertic(Notes, Type) :-
   length(Notes, L),
   (  L > 1,
      next(==, Notes),
      Type = still
      ;
      L > 2,
      (  next(<, Notes),
         Type = up
         ;
         next(>, Notes),
         Type = down  )
      ;
      L >= 3,
      alt(Notes),
      Type = alt  ), !.

/* next(+Operator, +Notes)
      Succeeds if each successive note in Notes progresses
      according to Operator. Auxiliary notes/3 compares each note
      to its successor.
*/
next(_, []).
next(Operator, [Note|Notes]) :-
   next(Operator, Note, Notes).
next(_, _, []) :- !.
next(Operator, Note, [Next|Notes]) :-
   Comparison =.. [Operator,Note,Next],
   Comparison,
   next(Operator, Next, Notes).

/* alt(+Notes)
      Succeeds if all notes in Notes go alternatingly up and down
      or vice versa.
*/
alt([]).
alt([N1,N2|Notes]) :-
   alt(N1, N2, Notes).
alt(_, _, []) :- !.
alt(N1, N2, [N3|List]) :-
   (  N1 < N2,
      N2 > N3
      ;
      N1 > N2,
      N2 < N3  ), !,
   alt(N2, N3, List).
