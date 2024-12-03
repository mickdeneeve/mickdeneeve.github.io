/* FILE:      otj.pl
   PURPOSE:   Compute semantics for Beethoven's Ode to Joy
   REQUIRES:  SWI Prolog (https://www.swi-prolog.org)
   AUTHOR:    Mick de Neeve <mick@live.nl>
   INSTITUTE: University of Amsterdam
   DATE:      December 3, 2024
*/

:- ensure_loaded([forces, lists]).

/* otj(-NotePartitions, -HarmonyPartitions)
      Note partitions and accompanying harmony for the first
      three measures of the second half of Schlenker's example 5
      (Ode to Joy) in his Prolegomena to Music Semantics (2019)
*/
otj([[2,2], [2,3,4], [4,4], [4,3,2,1], [0,0], [0,1,2]],
    [[0,0], [0,0,0], [0,4], [4,4,4,4], [0,0], [0,0,0]]).

/* map(+Name, -ValueList, -MapList)
      Specifies mapping between a feature in the world and one
      of Larson's forces, and/or the music's harmonic function.    
*/
map(visibility, [4,0], [decreasing,increasing]).
map(luminosity, [0.79,1,1.26,1.27], [none,dimmed,bright,bright]).
map(cloudcover, [down,still,up], [none,partial,full]).

/* map(+Name=ForceValue, -MapValue)
      Assigns a value specified in map/3 to a named feature.
*/
map(FName=FVal, FMap) :-
   map(FName, FVals, FMaps),
   nth0(NF, FVals, FVal),
   nth0(NF, FMaps, FMap).

/* events(+NotePartitions, +HarmonyPartitions, -MusicalEvents)
      Creates musical events by assigning Larson's forces and
      the harmonic function to note partitions.
*/
events([], [], []).
events([Notes|NParts], [Harms|HParts], [[H,K,I]|Events]) :-
   last(Harms, H),
   kmagnetic(imaj, Notes, K),
   inertic(Notes, I),
   events(NParts, HParts, Events).

/* events(+MusicalEvents, -WorldEvents)
      Applies mappings to musical events to create world events.
*/
events([], []).
events([[K,I,H]|E], [[KName=KMap,IName=IMap,HName=HMap]|W]) :-
   [visibility,luminosity,cloudcover] = [KName,IName,HName],
   map(KName=K, KMap),
   map(IName=I, IMap),
   map(HName=H, HMap),
   events(E, W), !.
