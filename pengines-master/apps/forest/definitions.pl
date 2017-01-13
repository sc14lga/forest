:- module(definitions, 
    [  rule/2 ]).

%% Preliminary Declarations
:- op( 100, xfx, ==>  ).  % Define a special operator to make the rules more readable.


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

%% General logical and set-theoretic rules for precisifications
rule( logic, [[P, subclass,C1,C2], [P, subclass,C2,C3]]  ==>  [P, subclass, C1, C3] ).
rule( logic, [[P, C1,X], [P, subclass,C1,C2]]  ==>  [P, C2, X] ).