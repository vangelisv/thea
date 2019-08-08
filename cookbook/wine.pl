:- [thea].                                      % setup paths
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_to_prolog_dlp')).

demo :-
   load_axioms('testfiles/wine.owl'),
   use_labels_for_IRIs,
   save_axioms('wine.pl',dlp,
               [ no_base(_),
                 write_directives(table),
                 write_directives(discontiguous),
                 write_directives(dummy_fact)
               ]),
   [wine],
   forall(light_us_desert_wine(Wine),
          format('~p is a light US dessert wine~n', [Wine])).

light_us_desert_wine(Wine) :-
   'DessertWine'(Wine),
   locatedIn(Wine,'USRegion'),
   hasBody(Wine,'Light').
