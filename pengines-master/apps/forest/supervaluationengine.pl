:- module(supervaluationengine, 
    [ supervaluate/1 ]).

:- op( 100, xfx, ==>  ).  % Define a special operator to make the rules more readable.

:- use_module(definitions).


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
supervaluate([(Prem ==>[_,_,R])|CRs], SuperTrue, R) :- append(Prem, T, SuperTrue), supervaluate(CRs, T, R).
%%supervaluate([(Prem ==>[P,C,R])|CRs], SuperTrue, R) :- append(Prem, T, SuperTrue), supervaluate(CRs, T, R).

