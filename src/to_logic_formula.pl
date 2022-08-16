% to_logic_formula(+Clause, -Formula)
% Converts grounded program to a logic formula describing Clause.
% ~(x /\ y) \/ z = or(not(and(x, y)), z)
to_logic_formula(Clause, F).