/* -*- Mode: Prolog -*- */

:- use_module(owl2_model,[]).

%% class(?Class,?Deprecated,?Complete,?Annotations, ?Descriptions)
owl_parser:class(C,false,true,[],Descs) :-
        owl2_model:class(C),
        equivalentClasses(ECL),
        select(C,ECL,Descs).

        
%% TODO: this is just an example so far...

