:- multifile term//1.

% non-terminals - class expressions
term(T)                           --> qual_expr(T) ; anat_expr(T).
qual_expr(Q and qualityOf some A) --> qual(Q),[of],anat_expr(A).
anat_expr(P and partOf some W)    --> anat(W),anat_expr(P).
anat_expr(A)                      --> anat(A).

% terminals - named classes
anat(A)                           -->
    {entailed(subClassOf(A,anatomical_entity)),
     labelAnnotation_value(A,Label)},
    [Label].
qual(Q)                           -->
    {entailed(subClassOf(Q,quality)),
     labelAnnotation_value(Q,Label)},
     [Label].

