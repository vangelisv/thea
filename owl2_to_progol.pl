/* -*- Mode: Prolog -*- */

:- module(owl2_to_progol,
          [
           write_progol_mode_decls/1,
           write_progol_facts/3
           ]).

:- use_module(owl2_model).
:- use_module(owl2_basic_reasoner).

%% write_progol_facts(+Head:term,+Goal:callable)
%
% Example: write_progol_facts(has_disease_type(H,DT), (propertyAssertion(inheres_in,D,H),classAssertion(D,DT)), disease_type(DT))
% writes:
% =|has_disease_type(human1,alzheimers). ... disease_type(alzheimers)|=
write_progol_facts(Head,Goal,TypeDecl) :-
        setof(TypeDecl,Goal^Goal,TypeDecls),
        forall(member(TypeDecl,TypeDecls),
               format('~q.~n',[TypeDecl])),
        forall(Goal,format('~q.~n',[Head])).

%% write_progol_mode_decls(+Roots:list)
%
% progol typing system means we have a general type for everything
%
% Example:
% =|:- modeb(1,brainstem(+object))?|=
write_progol_mode_decls(Roots) :-
        forall(class(X),
               write_progol_mode_decl(class(X),Roots)).

write_progol_mode_decl(class(X),Roots) :-
        entailed(subClassOf(X,Root)),
        member(Root,Roots),
        format(':- modeb(1,~q(+~q))?~n',[X,Root]),
        !.
write_progol_mode_decl(_,_).

/** <module> generates ProGol ILP programs from OWL2 ontologies

  ---+ Synopsis

==
:- use_module(bio(owl2_to_progol)).

% 
demo:-
  nl.
  

==

---+ Details



*/
