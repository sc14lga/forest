:- module(engine, 
    [ infer/1, describe/1]).


%% Preliminary Declarations
:- use_module(library(lists)).
:- use_module(definitions).
:- use_module(dataproxy).


:- op( 100, xfx, ==>  ).  % Define a special operator to make the rules more readable.


%% Define how to interpret 'test' conditions in rule preconditions:
test( X < Y ) :- X < Y.
test( X > Y ) :- X > Y.

%%% MAKE THE OR or([]) :- false.


%% THE INFERENCE MECHANISM

%% applyrule: check if premisses are satisfiable. If so assert the conclusion.
applyrule( [] ==> Conc ) :- % If no premisses, assert conclusion (unless already known).
    not( fact( Conc ) ),
    assert( fact( Conc ) ).

applyrule( [-(P) | Rest] ==> Conc ) :-   % Check negated premiss is not a fact
    \+(fact( P )),
    applyrule( Rest ==> Conc ).

applyrule( [test(T) | Rest] ==> Conc ) :-  % Evaluate a test condition.
     ground(T), test( T ),
     applyrule( Rest ==> Conc ).


applyrule( [Prem | Rest] ==> Conc ) :-     % Look for fact matching first premiss
     fact( Prem ),
     applyrule( Rest ==> Conc ).

%% infer applies all rules to all currently stored facts.
infer :-
         bagof( i, R^Type^( rule(Type, R), applyrule(R)), Infs ),
         length( Infs, Len ),
         Len > 0.  % fail if no inferences found.

%% infer/1 repeatedly calls infer up to a given inference depth limit.
infer( Limit ) :- infer( 1, Limit ).
infer( Depth, Limit ) :- Depth>Limit, !.
infer( Depth, Limit ) :- infer, !, 
                              Next is Depth + 1, infer( Next, Limit ).
infer( _, _ ).          

%% Useful Display Predicates
%% Show all facts 
allfacts :-  write('=== ALL KNOWN FACTS ==='), nl, (fact(F), write(F), nl, fail) ; true.
%% Show all facts involving terms in list L
describe(L) :- L=[] ; (L = [H|T], describe(H), describe(T)).
describe(X) :- write('=== Facts involving: '), write( X ), write(' ==='), nl, 
              ( (fact(F), member(X,F), write(F), nl, fail) ; true ).

show_inferences :- true. %% Change this to false to hide inference output.

