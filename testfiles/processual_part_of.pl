
% if part_of holds for a process, the specific
% relation is processual_part_of
processual_part_of(P,W) :- part_of(P,W),process(W).

% range of processual_part_of is processs
%process(W) :- processual_part_of(_,P).

part_of(p1,p2).
part_of(ob1,ob2).
process(p2).
object(ob2).


