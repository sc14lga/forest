:- module(forest, 
    [  color/1
    ]).

color([Prec, Concept, LatN, LatD, LonN, LonD]):-fact([Prec, Concept, Record]), swarecord(Record,LatN, LatD, LonN, LonD,_,_,_,_,_,_,_,_,_,_,_,_,_).
