:- module(forest, 
	[  color/1,
	   supervaluate/1,
	   infer/1,
	   fact/1
	]).


%% Preliminary Declarations
:- use_module(library(lists)).
:- op( 100, xfx, ==>  ).  % Define a special operator to make the rules more readable.
:- dynamic fact/1.        % fact/1 predicate can be altered while program is running.
:- retractall( fact(_) ). % Clear previously stored facts.


%% SPECIFY INFERENCE RULES

%% General supervaluation logic rules 
%rule( supervaluation, [[supertruth|L], precisification(P)] ==>  [P|L] ).

%supertruth(concept, instance) :- fact(P, concept, instance).
% Instances(records), List :- Precisification, Concept, ...
%classifications_of_record(I,L) :- findall([P,C,I], (fact([P,C,I]), precisification(P)), L).

% Instances(records), List :- Precisification, Concept, ...
%supertruth_classification(C, I) :- classifications_of_record(I, L),
%    findall([P,C,I], (fact([P,C,I]), precisification(P)), LC),
%    L is LC,
%    assert(fact([supertruth, C, I])).

%supervaluation :- findall( C, (fact([P,C|L])), Concepts),
%    findall( R, (rule(taxonomy, R)), Rules).

%conceptRules(Concept, Rules, ConceptRules)
conceptRules(C, CRs) :- findall( R, (rule(taxonomy, R)), Rs), conceptRules(C, Rs, CRs).
%
conceptRules(_,[],[]).
conceptRules(C, [(Prem ==> [P,C,R])|Rs], [(Prem ==> [P,C,R])|CRs]) :- conceptRules(C,Rs,CRs).
conceptRules(C, [(_ ==> [_,C2,_])|Rs], CRs) :-  not(C2 = C), conceptRules(C,Rs,CRs).

supervaluate(Concept) :- conceptRules(Concept, CRs), 
    						supervaluate(CRs, SuperTrue, Record),
    						assert(rule(taxonomys, SuperTrue ==> [supertrue, Concept, Record])).

supervaluate([], [], _).
supervaluate([(Prem ==>[P,C,R])|CRs], SuperTrue, R) :- append(Prem, T, SuperTrue), supervaluate(CRs, T, R).

%rule( supervaluation, [[prec, C1,X], [prec, subclass,C1,C2]]  ==>  [prec, C2, X] ).

%% General logical and set-theoretic rules for precisifications
rule( logic, [[P, subclass,C1,C2], [P, subclass,C2,C3]]  ==>  [P, subclass, C1, C3] ).
rule( logic, [[P, C1,X], [P, subclass,C1,C2]]  ==>  [P, C2, X] ).


%% Taxanomic relationships regarding the concept vocabulary
rule( taxonomy, [[abox, tree_cover,R,X], test(X>2), [abox, forest_use, R], -[abox, plantation,R]]   ==> [prec_fao, forest, R] ).
rule( taxonomy, [[abox, tree_cover,R,X], test(X<3), [abox, shrub_cover,R,Y], test(Y>10)]   ==> [prec_fao, shrubland, R] ).

rule( taxonomy, [[abox, tree_cover,R,X], test(X<3), [abox, shrub_cover,R,Y],test(Y<11)]   ==> [prec_fao, other_land, R] ).
rule( taxonomy, [[abox, tree_cover,R,X], test(X>2), -[abox, forest_use, R]]   ==> [prec_fao, other_land, R] ).
rule( taxonomy, [[abox, tree_cover,R,X], test(X>2), [abox, plantation, R]]   ==> [prec_fao, other_land, R] ).

rule( taxonomy, [[abox, tree_cover,R,X], test(X>7), [abox, forest_use, R]]   ==> [prec_landclass, forest, R] ).
rule( taxonomy, [[abox, tree_cover,R,X], test(X<8), [abox, shrub_cover,R,Y], test(Y>7)]   ==> [prec_landclass, shrubland, R] ).
rule( taxonomy, [[abox, tree_cover,R,X], test(X<8), [abox, shrub_cover,R,Y], test(Y<8)] ==> [prec_landclass, other_land, R] ).
rule( taxonomy, [[abox, tree_cover,R,X], test(X>7), -[abox, forest_use, R]] ==> [prec_landclass, other_land, R] ).


rule( taxonomy, [[abox, num_trees,R,X], test(X>5), [abox, forest_use, R]] ==> [prec_numveg, forest, R] ).
rule( taxonomy, [[abox, num_trees,R,X], test(X<6), [abox, num_shrubs,R,Y], test(Y>5)]   ==> [prec_numveg, shrubland, R] ).
rule( taxonomy, [[abox, num_trees,R,X], test(X<6), [abox, num_shrubs,R,Y], test(Y<6)] ==> [prec_numveg, other_land, R] ).



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

show_inferences :- false. %% Change this to false to hide inference output.

/** <examples>
 %% You can add your own queries here.
 
 %% Display known facts about a thing or list of things:
?- describe( champ ).
?- describe( [pete,rex,tweety] ).

?- allfacts. %% Display all known facts.

%% Compute inferences up to a given depth:
?- infer(1).
?- infer(2). 
?- infer(5). 

%% Compute inferences to a given level; then display known facts (including those inferred):
?- infer(1), describe([rex,champ]).
?- infer(3), describe([tweety]).    
?- infer(2), allfacts.
*/

%% DATA HANDLER

color([Prec, Concept, LatN, LatD, LonN, LonD]):-fact([Prec, Concept, Record]), swarecord(Record,LatN, LatD, LonN, LonD,_,_,_,_,_,_,_,_,_,_,_,_,_).

%% value(Record, Attribute, Value)
fact([abox, tree_cover, R, X]) :- swarecord(R,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_).
fact([abox, shrub_cover, R, X]) :- swarecord(R,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_).
fact([abox, num_trees, R, 31]) :- swarecord(R,_,_,_,_,_,_,_,_,_,_,_,_,_,t,_,_,_).
fact([abox, num_trees, R, X]) :- swarecord(R,_,_,_,_,_,_,_,_,_,_,_,_,_,f,X,_,_).
fact([abox, num_shrubs, R, 31]) :- swarecord(R,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,t,_).
fact([abox, num_shrubs, R, X]) :- swarecord(R,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,f,X).
fact([abox, forest_use, R]) :- swarecord(R,_,_,_,_,grassland,_,_,_,_,_,_,_,_,_,_,_,_).
fact([abox, forest_use, R]) :- swarecord(R,_,_,_,_,cropland,_,_,_,_,_,_,_,_,_,_,_,_).
fact([abox, plantation, R]) :- swarecord(R,_,_,_,_,_,_,_,_,_,_,_,_,plantation_other,_,_,_,_).

swarecord(swa16913,79,37115589,26,41545004,cropland,india,oltc,30,40,0,90,cropland,cropland_nonirrigated_floodplain,f,4,f,6). 
swarecord(swa11193,54,46958672,36,8640246,cropland,iran,ol,0,0,0,80,cropland,cropland_perennial,f,0,f,0). 
swarecord(swa2636,66,98060334,32,13897604,grassland,afghanistan,fo,0,0,0,30,grassland,grassland,f,0,f,0). 
swarecord(swa10051,79,45281275,13,32636753,cropland,india,ol,0,0,100,0,forest,plantation_other,t,0,f,0
). swarecord(swa13132,51,43076578,31,61130564,forest,iran,fo,0,0,100,0,forest,plantation_other,t,0,t,0
). swarecord(swa9631,82,01867069,16,67456826,otherland,india,ol,0,0,100,0,other,palm,f,0,f,0
). swarecord(swa19942,79,37762376,10,46217908,otherland,india,ol,0,0,100,0,other,palm,f,0,f,0
). swarecord(swa19953,79,27332935,10,31099262,cropland,india,ol,0,0,100,0,other,palm,f,0,f,0
). swarecord(swa9557,81,94752412,17,17771465,cropland,india,ol,0,0,100,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa10378,78,39801094,10,22906626,cropland,india,oltc,20,0,80,0,cropland,cropland_perennial,t,0,f,0
). swarecord(swa10034,77,28257285,12,9547345,cropland,india,oltc,30,0,70,0,other,palm,f,6,f,0
). swarecord(swa12734,67,68048078,36,70259735,cropland,afghanistan,ol,0,0,70,0,forest,plantation_other,f,0,f,0
). swarecord(swa19669,78,27027051,16,32233883,cropland,india,owl,0,20,60,0,cropland,cropland_nonirrigated_rainfed,f,1,f,20
). swarecord(swa7266,82,0631373,25,81798055,cropland,india,ol,0,0,60,0,cropland,cropland_orchard,f,0,f,0
). swarecord(swa10321,77,72643044,11,17741771,otherland,india,oltc,0,20,50,0,other,palm,f,21,f,0
). swarecord(swa10092,78,72787231,12,9447632,grassland,india,ol,0,0,50,0,grassland,grassland,f,0,f,0
). swarecord(swa10105,77,47849542,12,59388125,cropland,india,ol,0,0,50,0,other,palm,f,0,f,0
). swarecord(swa10358,78,41633657,10,90145525,cropland,india,oltc,0,0,50,0,cropland,cropland_perennial,f,5,t,0
). swarecord(swa10324,78,10183639,11,24559984,cropland,india,ol,0,0,50,0,forest,plantation_other,f,0,f,0
). swarecord(swa10015,77,89675101,13,19701013,cropland,india,oltc,20,20,40,0,cropland,cropland_perennial,f,1,t,0
). swarecord(swa10159,78,66454109,12,53809587,cropland,india,ol,30,0,40,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa19077,73,2687205,17,84767819,cropland,india,oltc,20,0,40,0,cropland,cropland_perennial,f,3,f,0
). swarecord(swa9852,77,22355354,13,99713955,settlement,india,ol,30,0,40,0,settlement,settlement_urban,f,2,f,0
). swarecord(swa605,50,42150855,32,62460528,otherland,iran,owl,30,0,40,0,shrubland,shrubland_trees,f,5,t,0
). swarecord(swa18296,70,48026003,20,84957837,cropland,india,oltc,40,0,40,0,cropland,cropland_nonirrigated_rainfed,f,4,f,0
). swarecord(swa10082,77,45558838,12,72189118,cropland,india,oltc,40,0,40,0,cropland,cropland_perennial,f,6,f,0
). swarecord(swa9809,77,56338621,14,31999407,cropland,india,oltc,0,0,40,0,cropland,cropland_irrigated,f,0,f,0
). swarecord(swa10199,78,32740336,12,21578034,cropland,india,ol,0,0,40,0,cropland,cropland_perennial,t,0,f,0
). swarecord(swa1686,51,06317516,29,28987929,cropland,iran,ol,0,0,40,0,other,palm,f,0,f,0
). swarecord(swa9588,81,58262846,16,87507223,cropland,india,ol,0,0,40,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa16256,74,512977,27,41716141,cropland,india,ol,0,0,40,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa9428,83,73427322,18,14870364,cropland,india,ol,0,0,40,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa10011,77,38659776,13,10565146,settlement,india,ol,0,0,40,0,settlement,settlement_urban,f,0,f,0
). swarecord(swa10237,78,37052249,11,95814352,cropland,india,ol,0,0,40,0,cropland,cropland_nonirrigated_rainfed,f,0,f,0
). swarecord(swa9582,80,78303802,16,76835237,cropland,india,ol,0,0,40,0,cropland,cropland_irrigated,f,0,f,0
). swarecord(swa9608,81,73435391,16,76646591,otherland,india,oltc,0,0,40,0,grassland,grassland_trees,t,0,f,0
). swarecord(swa6109,72,94883043,26,19662044,cropland,india,ol,0,40,30,0,cropland,cropland_perennial,f,0,f,5
). swarecord(swa3136,71,08635335,32,19691738,cropland,pakistan,oltc,40,30,30,0,cropland,cropland_perennial,t,0,f,10
). swarecord(swa10163,79,17280787,12,62298666,cropland,india,oltc,40,30,30,0,cropland,cropland_nonirrigated_rainfed,f,4,f,3
). swarecord(swa10354,77,9172326,10,81171356,otherland,india,ol,0,20,30,0,other,palm,f,0,t,0
). swarecord(swa9966,77,34024469,13,36086284,cropland,india,owl,20,0,30,0,cropland,cropland_nonirrigated_rainfed,f,11,f,0
). swarecord(swa9565,80,76363441,16,89393685,cropland,india,oltc,20,0,30,0,cropland,cropland_nonirrigated_rainfed,f,3,f,0
). swarecord(swa20002,78,89954036,9,440525106,settlement,india,oltc,20,0,30,0,settlement,settlement_village,f,9,f,0
). swarecord(swa10088,78,21825805,12,85771645,cropland,india,oltc,20,0,30,0,cropland,cropland_perennial,f,2,f,0
). swarecord(swa12545,54,91748672,33,79448128,cropland,iran,owl,0,0,30,0,cropland,cropland_perennial,f,19,f,7
). swarecord(swa8762,81,73058098,21,44947332,settlement,india,ol,0,0,30,0,settlement,settlement_infrastructure,f,0,f,0
). swarecord(swa10080,80,11388297,13,29834009,cropland,india,oltc,0,0,30,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa10116,78,87636383,12,83777385,cropland,india,ol,0,0,30,0,cropland,cropland_irrigated,f,0,f,0
). swarecord(swa10348,78,14522502,10,98661554,cropland,india,ol,0,0,30,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa10218,78,34896293,12,08696193,cropland,india,ol,0,0,30,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa9586,81,31582882,16,84030743,cropland,india,ol,0,0,30,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa10286,77,55637935,11,41295598,cropland,india,ol,0,0,30,0,cropland,cropland_nonirrigated_floodplain,f,0,f,0
). swarecord(swa10179,78,43223675,12,36615832,cropland,india,ol,0,0,30,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa9552,81,27836907,17,09147638,cropland,india,ol,0,0,30,0,cropland,cropland_nonirrigated_rainfed,f,0,f,0
). swarecord(swa10109,77,98595372,12,68497042,cropland,india,oltc,20,30,20,0,cropland,cropland_irrigated,f,7,f,3
). swarecord(swa10377,78,27404343,10,20669821,grassland,india,oltc,40,30,20,0,shrubland,shrubland_trees,f,7,t,0
). swarecord(swa3443,70,84542519,31,7032033,cropland,pakistan,oltc,0,30,20,0,cropland,cropland_perennial,f,8,t,0
). swarecord(swa17653,69,73537699,22,8540791,cropland,india,ol,20,20,20,0,cropland,cropland_perennial,f,9,f,1
). swarecord(swa10327,78,47751184,11,31243449,otherland,india,owl,30,20,20,0,shrubland,shrubland_trees,f,5,f,15
). swarecord(swa19904,78,56833152,10,72412782,settlement,india,owl,0,20,20,0,settlement,settlement_village,f,0,t,0
). swarecord(swa10035,77,4097743,12,97791103,cropland,india,oltc,10,10,20,0,cropland,cropland_irrigated,f,1,f,1
). swarecord(swa19905,78,69310751,10,74595688,settlement,india,oltc,20,10,20,0,settlement,settlement_urban,f,3,f,1
). swarecord(swa10134,78,26191618,12,6011576,cropland,india,oltc,20,0,20,0,cropland,cropland_perennial,f,6,f,0
). swarecord(swa18233,70,27382717,21,05951465,cropland,india,owl,20,0,20,0,cropland,cropland_orchard,f,16,f,0
). swarecord(swa10149,80,17236329,12,91323233,settlement,india,owl,20,0,20,0,settlement,settlement_urban,f,5,f,0
). swarecord(swa3863,58,27565875,27,8707208,cropland,iran,oltc,30,0,20,0,cropland,cropland_perennial,t,0,f,0
). swarecord(swa10093,78,85534325,12,96605327,settlement,india,oltc,30,0,20,0,settlement,settlement_village,f,10,f,0
). swarecord(swa19950,78,90034885,10,2468529,cropland,india,ol,10,0,20,0,cropland,cropland_perennial,f,1,f,0
). swarecord(swa10236,78,24439903,11,93604497,cropland,india,ol,10,0,20,0,cropland,cropland_fallow,f,3,f,0
). swarecord(swa10056,80,09420986,13,42635002,cropland,india,oltc,10,0,20,0,cropland,cropland_perennial,f,1,f,0
). swarecord(swa10323,77,97652141,11,22296229,cropland,india,oltc,10,0,20,0,cropland,cropland_perennial,f,1,f,0
). swarecord(swa7233,85,47062684,26,25698722,cropland,india,oltc,20,0,20,0,cropland,cropland_nonirrigated_rainfed,f,2,f,0
). swarecord(swa10154,78,03042033,12,42841157,cropland,india,ol,20,0,20,0,cropland,cropland_perennial,f,3,f,0
). swarecord(swa10094,78,98281419,12,98707385,cropland,india,oltc,20,0,20,0,cropland,cropland_perennial,f,3,f,0
). swarecord(swa10215,77,97032303,12,02012728,cropland,india,oltc,30,0,20,0,cropland,cropland_perennial,f,8,f,0
). swarecord(swa9317,84,20238532,18,83025544,settlement,india,oltc,40,0,20,0,settlement,settlement_village,f,6,f,0
). swarecord(swa9680,80,87924759,16,13881302,cropland,india,ol,0,0,20,0,cropland,cropland_nonirrigated_rainfed,f,0,f,0
). swarecord(swa9970,77,85174542,13,4522215,settlement,india,oltc,0,0,20,0,settlement,settlement_village,f,5,f,0
). swarecord(swa4556,60,6218786,27,14982279,cropland,iran,ol,0,0,20,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa9626,81,35328856,16,58859948,cropland,india,oltc,0,0,20,0,cropland,cropland_irrigated,f,9,f,0
). swarecord(swa10307,78,08000733,11,37495724,cropland,india,ol,0,0,20,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa10073,79,21754398,13,15712493,cropland,india,ol,0,0,20,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa10252,77,88839668,11,73985291,cropland,india,ol,0,0,20,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa4492,62,85518023,27,92731466,grassland,pakistan,ol,0,0,20,0,other,palm,f,0,f,0
). swarecord(swa9999,78,77018296,13,47782349,cropland,india,ol,0,0,20,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa10310,78,45622177,11,4417919,cropland,india,oltc,0,0,20,0,cropland,cropland_perennial,f,12,f,0
). swarecord(swa17609,69,41979883,22,91013397,cropland,india,ol,0,0,20,0,cropland,cropland_irrigated,f,0,f,0
). swarecord(swa10305,77,82937737,11,32968215,cropland,india,ol,0,0,20,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa9570,81,43063351,16,98367855,cropland,india,ol,0,0,20,0,cropland,cropland_perennial,f,0,f,0
). swarecord(swa10013,77,64153964,13,15173504,settlement,india,oltc,0,0,20,0,settlement,settlement_urban,f,1,f,0
). swarecord(swa9649,81,63760535,16,49751031,wetland,india,iwb,0,0,20,0,wetland,wetland_lake_pool_permanent,f,0,f,0
). swarecord(swa10253,78,01425065,11,76249045,settlement,india,oltc,30,30,10,0,settlement,settlement_urban,f,7,f,2
). swarecord(swa6583,76,78077394,31,61103615,cropland,india,oltc,30,20,10,0,cropland,cropland_nonirrigated_rainfed,f,11,f,15
). swarecord(swa19550,76,72364108,16,57539425,settlement,india,oltc,50,20,10,0,settlement,settlement_village,t,0,f,7
). 