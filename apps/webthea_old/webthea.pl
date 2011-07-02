/* -*- Mode: Prolog -*- */

:- module(webthea,
          [
           start_server/0,
           start_server/1,
           start_server/2
          ]).

:- use_module(library('thea/owl2_model')).
:- use_module(library('thea/owl2_io')).
:- use_module(library('thea/owl2_to_prolog_dlp')).

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_parameters')).

start_server :-
        start_server(9000,[]).

start_server(Port) :-
        start_server(Port,[]).

start_server(Port,Opts) :-
        forall(member(load(OwlFile),Opts),
              load_axioms(OwlFile)),
        http_server(http_dispatch, [port(Port)]).

:- http_handler('/', root, []).
:- http_handler('/hello/world', hello_world, []).
:- http_handler('/view', view_entity, []).
:- http_handler('/all/owl', show_all(owl), []).
:- http_handler('/all/dlp', show_all(dlp), []).
:- http_handler('/all/owlpl', show_all(owlpl), []).

param(title, [optional(true)]).
param(name,  [length >= 2 ]).
param(age,   [integer]).
param(url,   [optional(true),default('http://')]).
param(open,  [zero_or_more]).
param(entity,  []).

root(Request) :-
        http_parameters(Request,
                        [ url(Url),
                          open(OpenClasses)
                        ],
                        [ attribute_declarations(param)
                        ]),
        reply_html_page([ title('Demo server')
                        ],
                        [ p(a(href('hello/world'), hello)),
                          \navbar,
                          form([p('Enter URL'),
                                input([type(textfield),name(url),rows(1),value(Url)],'URL'),
                                p('route:'),
                                select([name(route)],
                                       [option([value(x)])]),
                               input([name(submit),type(submit),value(convert)])]),
                          \add_axiom_form,
                          \subclass_tree(OpenClasses)
                        
                        ]).

hello_world(Request) :-
        http_parameters(Request,
                        [ title(Title),
                          name(Name),
                          age(Age)
                        ],
                        [ attribute_declarations(param)
                        ]),
        reply_html_page([ title('Hello World')
                        ],
                        [ h1('Hello World'),
                          p('This is my first page')
                        ]).

show_all(Fmt,Request) :-
        tmp_file(Fmt,File),
        save_axioms(File,Fmt),
        read_file_to_codes(File,Codes,[]),
        atom_codes(A,Codes),
        reply_html_page([ title(Fmt)
                        ],
                        [ pre(A)
                        ]).

view_entity(Request) :-
        http_parameters(Request,
                        [ entity(E)
                        ],
                        [ attribute_declarations(param)
                        ]),
        reply_html_page([ title(E)],
                        [ h2('Axioms'),
                          \axiom_infos(E),
                          h2('Referenced in'),
                          \axiom_infos_referencing(E)
                        ]).

axiom_infos(E) -->
        {findall(\axiom_info(A),axiom_directly_about(A,E),L)},
        html(L).
axiom_infos_referencing(E) -->
        {findall(\axiom_info(A),axiom_references(A,E),L)},
        html(L).

        


navbar --> html([p(a(href('all/owl'),owl)),
                 p(a(href('all/dlp'),dlp)),
                 p(a(href('all/owlpl'),owlpl))]).

add_axiom_form -->
        {
         findall(option([value(A)]),axiompred(A/_),Opts)
         },
        html([
              form([p('Add Axiom'),
                    select([name(add_axiom)],
                           Opts),
                    input([name(submit),type(submit),value(add)])])
             ]).

subclass_tree(OpenClasses) -->
        {findall(\subclass_tree(C,OpenClasses),(class(C),\+subClassOf(C,_)),Elts)},
        html([p(open),
              p(OpenClasses)|
              Elts]
            ).

subclass_tree(Class,OpenClasses) -->
        {
         (   member(Class,OpenClasses)
         ->  findall(\subclass_tree(SubClass,OpenClasses),
                     subClassOf(SubClass,Class),
                     ChildElts)
         ;   (   \+ \+ subClassOf(_,Class)
             ->  findall(open(C),member(C,[Class|OpenClasses]),Params),
                 ChildElts=[' ',a(href(location_by_id(root)+Params),'+')]
             ;   ChildElts=[]))
         },
        html([ul(li([\class_info(Class)|
                     ChildElts]))]).

axiom_info(A) -->
        {A=..[P|L],
         findall(\entity_info(E),member(E,L),EL)},
        html([p(P),'(',p(EL),')']).

class_info(C) --> entity_info(C).

entity_info(C) -->
        {atom(C),
         display_label(C,Label)},
        !,
        html([' ',a(href(location_by_id(view_entity)+'?entity='+encode(C)),Label),' ']).
%        html(a(href(location_by_id(view_entity)+'entity='+encode(C)),C)).

entity_info(L) -->
        {is_list(L),
         findall(\entity_info(E),member(E,L),EL)},
        html(EL).

entity_info(A) -->
        {A=..[P|L],
         findall(\entity_info(E),member(E,L),EL)},
        html([p(P)|EL]).

display_label(C,Label) :- labelAnnotation_value(C,Label),!.
display_label(C,Label) :- concat_atom([_,Label],'#',C),!.
display_label(C,C).

            

/** <module> OWL server

---+ Synopsis

  Run this on command line:
==
swipl -g "[webthea],start_server(9000,[load('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine')])"
==

Then point your browser at

  http://localhost:9000
  
---+ Details

This is a web front end to the Thea OWL library. It will allow basic ontology browsing and editing.
  
Documentation will be online as part of the server

---+ Status

PRE-ALPHA. HIGHLY INCOMPLETE
  
*/
