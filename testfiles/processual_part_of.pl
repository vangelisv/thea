
% if part_of holds for a process, the specific
% relation is processual_part_of
processual_part_of(P,W) :- part_of(P,W),process(W).

% if part_of holds for an object, the specific
% relation is spatial_part_of
static_part_of(P,W) :- part_of(P,W),object(W).

part_of(p1,p2).
part_of(ob1,ob2).
process(p1).
process(p2).
object(ob1).
object(ob2).


% range of processual_part_of is processs
%process(W) :- processual_part_of(_,P).
