/* -*- Mode: Prolog -*- */

:- module(owl2_sqlmap_owlgres,
          [
           owlgres_query/2
          ]).

:- use_module(owl2_model).
:- use_module(owl2_metamodel).
:- use_module(owl2_io).
:- use_module(bio(sql_compiler)). % TODO - make independent
:- use_module(library(odbc)).

:- load_schema_defs(owlgres_sql_schema).

owl2_model:objectPropertyAssertion(P,A,B) <-
  object_role_assertion(PI,AI,BI),
  tbox_name(PI,_,_,_,P),
  individual_name(AI,A),
  individual_name(BI,B).

owl2_model:dataPropertyAssertion(P,A,Value) <-
  data_role_assertion(PI,AI,Value,_Datatype,_Lang),
  tbox_name(PI,_,_,_,P),
  individual_name(AI,A).

owl2_model:propertyAssertion(P,A,B) :- dataPropertyAssertion(P,A,B).
owl2_model:propertyAssertion(P,A,B) :- objectPropertyAssertion(P,A,B).

owl2_model:classAssertion(C,A) <-
  concept_assertion(CI,AI),
  tbox_name(CI,C),
  individual_name(AI,A).

% some of this code is copy-n-pasted from blipkit rdb_util
owlgres_query(Goal,Project) :-
        % these are standard env vars recommended in the owlgres docs
        getenv('OWLGRES_DB',DB),
        getenv('OWLGRES_USER',USER),
        getenv('OWLGRES_PASSWD',PASSWD),
        odbc_connect(DB,Dbh,[user(USER),alias(DB),password(PASSWD),open(once),silent(true)]),
        plterm_to_sqlterm(Project,Goal,SqlTerm),
        sqlterm2atom(SqlTerm,Sql),
        debug(sql,'SQL: ~w',[Sql]),
        odbc_prepare(Dbh,
                     Sql,
                     [],
                     Sth,
                     []),
        !,
        debug(sql,'prepared, now executing',[]),
        odbc_execute(Sth,[],RowTerm),
        debug(sql,'RowTerm: ~w',[RowTerm]),
        (   is_list(Project)
        ->  ProjectColVars=Project
        ;   compound(Project)
        ->  Project=..[_|ProjectColVars]
        ;   ProjectColVars=[Project]),
        RowTerm =.. [row|Vals],
        unify_project_cols(ProjectColVars,Vals).
        
%% unify_project_cols(+ProjectionVars,+ResultVals) is det
% assumes order is preserved
unify_project_cols([],[]).
unify_project_cols([Var|RestVars],Vals):-
	compound(Var),
	!,
	Var=..[_|InnerVars],
	append(InnerVars,RestVars,Vars),
	unify_project_cols(Vars,Vals).
unify_project_cols([Var|Vars],[Val|Vals]):-
        Var=Val,
        !,
        unify_project_cols(Vars,Vals).
unify_project_cols([Var|Vars],[Val|Vals]):-
        number(Var),
        atom_number(Val,Var),
        !,
        unify_project_cols(Vars,Vals).


% ----------------------------------------
% BRIDGE
% ----------------------------------------


/** <module> Transparent queries over OWLGRES database

---+ Synopsis

  Query all triples:
==
swipl -g "[owlgres/owl2_sqlmap_owlgres],owlgres_query(propertyAssertion(P,A,B),foo(P,A,B)),writeln(P-A-B),fail"  
==

---+ Details

Owlgres is an open source, scalable reasoner for OWL2.

See:
http://pellet.owldl.com/owlgres

With OWLGRES, the size of the ABox is not memory-bound

This module provides a mapping between an OWLGRES database and owl2_model.pl

So far this just maps the ABox. ie:

* classAssertion/2
* propertyAssertion/3

Note that objectPropertyAssertion/3 and  dataPropertyAssertion/3 map directly to tables in the owlgres schema.
This means we redefine propertyAssertion/3 to be the union of these (this inverts the pattern in owl2_model, where the strongly typed versions are intensional predicates)

==
plterm_to_sqlterm(foo(P,A,B),objectPropertyAssertion(P,A,B),SQL)
==

---++ Status

---+ Additional Information

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
