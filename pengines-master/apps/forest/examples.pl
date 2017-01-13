/** <examples>
 %% You can add your own queries here.
 
 %% Display known facts about a thing or list of things:
?- describe( forest ).
?- describe( [supertrue,forest] ).

?- allfacts. %% Display all known facts.

%% Compute inferences up to a given depth:
?- infer(1).

%% Compute inferences to a given level; then display known facts (including those inferred):
?- infer(2), allfacts.
*/
