%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   This in a SWI-prolog implementation   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
###########################
#       Demo runners      #
###########################
*/
run1:-
  run([begin,a,:=,5,while,a,>,0,begin,a,:=,a,-,1,write,a,end,end] ).

run2:-
  readFromFile('/Users/mikael/Dropbox/Skola/2012/HT12/PROP/A_2/program.pl').

/*
###########################
#  Run program from file  #
###########################
*/
readFromFile(FileIn):-
  open(FileIn, read, Stream),
  read_file(Stream, FileDataList),
  flatten(FileDataList, FlattenedDataList),  
  string_to_atom(FlattenedDataList, AsciiList),
  atomic_list_concat(ProgramList,' ', AsciiList),
  delete(ProgramList, '', FinalList),
  run(FinalList).
  
/*
#########################################################################
# Reading lines of loaded file, convers characters it to ASCII integer. #
#########################################################################
*/
read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[Head|Tail]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Head),
    read_file(Stream, Tail).

/*
###########################################
# Flatterns list to one list.             #
# This flatterns the program to oneline   #
###########################################
*/
flatten([],[]).

flatten([Head|InTail],Out) :-
	flatten(Head,FlatHead),  
	flatten(InTail,OutTail),
  append(FlatHead,OutTail,Out).

flatten([Head|InTail], [Head|OutTail]) :-
	Head \= [],
	Head \= [_|_],
	flatten(InTail,OutTail).

/*
#####################
# The main Program  #
#####################
*/
run(Program):-
  program(Program, _).

program(Program, FinalState) :-
	parse(ParseTree, Program, []),
	evaluate(ParseTree, [], FinalState).

parse(ParseTree) --> parse_block(ParseTree).

parse_block(block(_)) --> [begin], [end].
parse_block(block(ParseTree)) --> [begin], statement_list(ParseTree), [end].

statement_list([ParseTree]) --> statement(ParseTree).
statement_list([ParseTree1|ParseTree2]) --> statement(ParseTree1), statement_list(ParseTree2).

statement(statement(ParseTree)) --> parse_block(ParseTree)
  | [write], write_statement(ParseTree)
  | [read], read_statement(ParseTree)
	| [while], while_statement(ParseTree)
	| [if], if_statement(ParseTree)
	| assignment_statement(ParseTree).
	
/*
###############################################################
# The different statements that the language has implemented. #
###############################################################
*/
while_statement(while(Condition, Statement)) --> 
  condition(Condition), 
  statement(Statement).

if_statement(if(Condition, Statement, ElseStatement)) --> 
  condition(Condition), 
  [then],
  statement(Statement),
  [else], 
  statement(ElseStatement).
  
if_statement(if(Condition, Statement)) --> 
  condition(Condition), 
  [then], 
  statement(Statement).

write_statement(write(Expression)) --> 
  expression(Expression).

read_statement(read(variable(Variable))) --> 
  value(variable(Variable)).

assignment_statement(assign(Variable, Expression)) --> 
  data(Variable), 
  [:=], 
  expression(Expression).

/*
###################################
# What the statements consist of. #
###################################
*/
condition(condition(variable(Variable), Comparator, Expression)) --> 
  data(variable(Variable)),
  comparator(Comparator),
  expression(Expression).

expression([Value]) --> 
  data(Value).
expression([Value|Expression]) --> 
  data(Value), 
  rest_expression(Expression).

rest_expression([]) --> [].
rest_expression([Operator, Value|Expression]) --> 
  operator(Operator), 
  data(Value), 
  rest_expression(Expression).

/*
###########################################
# The datastructurs of the implementation #
###########################################
*/

%two number rules are needed cause the fileloader converts numbers to char. %  
data(number(X)) --> [Var], {not(number(Var)), atom_number(Var, X)}. 
data(number(Var)) --> [Var], {number(Var)}.
data(variable(Var)) --> [Var], {atom(Var), not(atom_number(Var, _))}.

/*
#################################################
# The derivation and assignment of the program. #
#################################################  
*/
operator(Op) --> 
	[+], {Op = operator(+)} |
	[-], {Op = operator(-)} |
	[*], {Op = operator(*)} |
	[/], {Op = operator(/)}.

comparator(Comp) --> 
	[<], {Comp = comperator(<)} |
	[>], {Comp = comperator(>)} |
	[<=], {Comp = comperator(=<)} |
	[=<], {Comp = comperator(=<)} |
	[>=], {Comp = comperator(>=)} |
	[=>], {Comp = comperator(>=)} |
	[<>], {Comp = comperator(<>)} |
	[=], {Comp = comperator(=)}.

/*
#####################################
# The evaluation of the parse three #
#####################################
*/
evaluate(statement(Statement), State, NewState) :-
	evaluate(Statement, State, NewState).

evaluate(block(Block), State, NewState) :-
	evaluate_block(Block, State, NewState).
	
evaluate(assign(variable(Var), Expression), State, NewState) :-
	evaluate_expression(Expression, State, Out),
	set_variable(Var, Out, State, NewState).
	
evaluate(while(Condition, Statement), State, NewState) :-
	(evaluate_condition(Condition, State) -> 
		evaluate(Statement, State, NewState2),
		evaluate(while(Condition, Statement), NewState2, NewState) ;
		NewState = State).
		
evaluate(if(Condition, Statement, ElseStatement), State, NewState) :-
	(evaluate_condition(Condition, State) -> 
		evaluate(Statement, State, NewState) ;
		evaluate(ElseStatement, State, NewState)).

evaluate(if(Condition, Statement), State, NewState) :-
	(evaluate_condition(Condition, State) -> 
		evaluate(Statement, State, NewState) ;
		NewState = State).
		
evaluate(write(Expression), State, State) :-
	evaluate_expression(Expression, State, Value),
	write(Value), nl.
	
evaluate(read(variable(Variable)), State, NewState) :-
	write(Variable), write(': '),
	read(NewValue),
	set_variable(Variable, NewValue, State, NewState).


evaluate_block([], State, State).
evaluate_block([statement(Statement)|Rest], OldState, NewState) :-
	evaluate(Statement, OldState, CurrentState),
	evaluate_block(Rest, CurrentState, NewState).

evaluate_condition(condition(variable(Variable), comperator(Comparator), Expression), State) :-
	get_variable(Variable, State, Value1),
	evaluate_expression(Expression, State, Value2),
	!,
	compares(Value1, Comparator, Value2).

compares(Value1, <, Value2) :-
	Value1 < Value2.
compares(Value1, >, Value2) :-
	Value1 > Value2.
compares(Value1, =<, Value2) :-
	Value1 =< Value2.
compares(Value1, >=, Value2) :-
	Value1 >= Value2.
compares(Value1, <>, Value2) :-
	Value1 =\= Value2.
compares(Value1, =, Value2) :-
	Value1 =:= Value2.

evaluate_term(number(Value), _, Value).
evaluate_term(variable(Value), State, Out) :-
	get_variable(Value, State, Out).
evaluate_term(parenthesis(Expression), State, Out) :-
	evaluate_expression(Expression, State, Out).
	
evaluate_expression([Term], State, Out) :-
	evaluate_term(Term, State, Out).
evaluate_expression([Term1, operator(Operator), Term2 | Rest], State, Out) :-
	calculate(Term1, Operator, Term2, State, OutTemp),
	evaluate_expression([number(OutTemp) | Rest], State, Out).

calculate(Term1, Operator, Term2, State, Out) :-
	evaluate_term(Term1, State, Value1),
	evaluate_term(Term2, State, Value2),
	calculate(Value1, Operator, Value2, Out).

calculate(Value1, +, Value2, Out) :-
	Out is Value1 + Value2.
calculate(Value1, -, Value2, Out) :-
	Out is Value1 - Value2.
calculate(Value1, *, Value2, Out) :-
	Out is Value1 * Value2.
calculate(Value1, /, Value2, Out) :-
	Out is Value1 / Value2.

set_variable(Variable, NewValue, [], [(Variable, NewValue)]).
set_variable(Variable, NewValue, [(Variable, _) | RestState], [(Variable, NewValue) | RestState]).
set_variable(Variable, NewValue, [(A, N) | RestState], [(A, N) | RestNewState]) :-
	set_variable(Variable, NewValue, RestState, RestNewState).

get_variable(_, [], 0).
get_variable(Variable, [(Variable, Out) | _], Out).
get_variable(Variable, [_ | Rest], Out) :-
	get_variable(Variable, Rest, Out).
	
not(P) :- call(P), !, fail. 
not(_).	