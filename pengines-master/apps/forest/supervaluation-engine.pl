:- module(supervaluation-engine, 
    [ 
       supervaluate/1,
    ]).

%%%%%%
%% conceptRules(Concept, Rules, ConceptRules)
%%  -- Finds among all the rules (Rs), those related to the Concept (C) and returns them in ConceptRules (CRs)
%%%%%%
conceptRules(C, CRs) :- findall( R, (rule(taxonomy, R)), Rs), conceptRules(C, Rs, CRs).
%First it finds all the taxonomic rules (Rs), then reduces with base case and recursion.
conceptRules(_,[],[]).
conceptRules(C, [(Prem ==> [P,C,R])|Rs], [(Prem ==> [P,C,R])|CRs]) :- conceptRules(C,Rs,CRs).
conceptRules(C, [(_ ==> [_,C2,_])|Rs], CRs) :-  not(C2 = C), conceptRules(C,Rs,CRs).

%%%%%%
%% supervaluate(Concept)
%%  -- Finds among all the rules (Rs), those related to the Concept (C) and returns them in ConceptRules (CRs)
%%%%%%
supervaluate(Concept) :- conceptRules(Concept, CRs), 
                            supervaluate(CRs, SuperTrue, Record),
                            assert(rule(taxonomy, SuperTrue ==> [supertrue, Concept, Record])).

supervaluate([], [], _).
supervaluate([(Prem ==>[P,C,R])|CRs], SuperTrue, R) :- append(Prem, T, SuperTrue), supervaluate(CRs, T, R).

%rule( supervaluation, [[prec, C1,X], [prec, subclass,C1,C2]]  ==>  [prec, C2, X] ).