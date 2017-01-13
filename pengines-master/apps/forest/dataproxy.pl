:- module(dataproxy, 
    [  
    fact/1
    ]).

:- use_module(library(lists)).
:- use_module(data).
:- dynamic fact/1.        % fact/1 predicate can be altered while program is running.
:- retractall( fact(_) ). % Clear previously stored facts.

%% DATA HANDLER

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

