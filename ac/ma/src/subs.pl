/* FILE:      subs.pl
   PURPOSE:   Apply harmonic mode substitutions to note sequences
   REQUIRES:  SWI Prolog (https://www.swi-prolog.org)
   AUTHOR:    Mick de Neeve <mick@live.nl>
   INSTITUTE: University of Amsterdam
   DATE:      December 3, 2024
*/

:- ensure_loaded([lists]).

/* stability(-StabilityList)
      Specifies degrees from most to least stable.
      NB: UNUSED SO FAR
*/
stability([0,4,2,5,1,3,6]).

/* subs(+NoteList/HarmonyList, +NewMode, +Index, -DegreesHarmLists)
      DegreesHarmLists is a list of lists with all solutions to
      finding contiguous stable applications of NewMode from Index
      using subs/5, and applying the result to update/6 to produce
      the new harmony lists.       
*/
subs(Notes/Harms, NewMode, Index, NewDegsHarms) :-
   degrees(Notes, Harms, Degs),
   findall(  NewDegs/NewHarms,
             (  subs(Degs,Harms,NewMode,Index,ISubs),
                update(Degs,Harms,ISubs,NewMode,NewDegs,NewHarms)
             ), NewDegsHarms  ).

/* subs(+DegreesList, +HarmonyList, +NewMode, +Index, -Subs)
      Subs is a list of substitutions, i.e. degrees to which
      NewMode can be applied, such that the substitutions are
      contiguous from Index and stable given NewMode.
*/
subs(Degs, Harms, NewMode, Index, ISubs) :-
   index(Degs, 0, IDegs),
   index(Harms, 0, IHarms),
   append(_Degs1, Degs2, IDegs),
   append(ISubs, _Degs3, Degs2),
   member(Index:_, ISubs),
   select(ISubs, IHarms, SubHarms),
   stable(ISubs, SubHarms, NewMode).

/* degrees(+Notes, +Harms, -Degrees)
      Degrees is a list of harmonic roles of the notes given the harmonic
      function numbers in Harms.
*/
degrees([], [], []).
degrees([Note|Notes], [Harm|Harms], [Degree|Degrees]) :-
   Degree is (Note-Harm) mod 7,
   degrees(Notes, Harms, Degrees).

/* stable(+Subs, +Harms, +NewMode)
      Succeeds if all suggested substitutions Subs, given the
      current harmonic sequence Harms, are stable under the new
      harmonic mode NewMode.
*/
stable([], [], _).
stable([_:Deg|DTail], [_:Harm|HTail], Mode) :-
   Sub is ((Deg+Harm)-Mode) mod 7,
   member(Sub, [0,4,2]), !,
   stable(DTail, HTail, Mode).

/* update(+Degrees, +Harms, +Subs, +NewMode, -NewDegrees, -NewHarms)
      Apply NewMode and Subs (according to given indeces I therein) to
      Degrees and Harms such that new substituted degrees NewDegrees are
      produced that are consistent with new harmonic functions NewHarms.
*/
update(Degrees, Harms, [], _, Degrees, Harms) :- !.
update(Degrees, Harms, [I:Sub|Subs], NewMode, NewDegrees, NewHarms) :-
   index(Harms, 0, IHarms),
   member(I:Harm, IHarms), !,
   NewSub is ((Sub+Harm)-NewMode) mod 7,
   replace(I, NewSub, Degrees, NextDegrees),
   replace(I, NewMode, Harms, NextHarms),
   update(NextDegrees, NextHarms, Subs, NewMode, NewDegrees, NewHarms).
