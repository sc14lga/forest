:- module(app_forest, []).
:- use_module(library(pengines)).

:- pengine_application(forest).
:- use_module(forest:forest).
:- use_module(forest:engine).
:- use_module(forest:supervaluationengine).
:- use_module(forest:dataproxy).
:- use_module(forest:data).
