-module(recmod).
-export([parse_transform/2]).

-record(recmod, 
	{
	  name,
	  rec,
	  parameters,
	  static = [{record_fields,0}],
	  functions = [],
	  extends,
	  extends_parameters
	 }).

parse_transform(Forms, _Options) ->
    {Forms1, St1} = forms(Forms, #recmod{}, fun form/2),
    {Forms2, St2} = forms(Forms1, St1, fun extension/2),
    {Forms3, St3} = forms(Forms2, St2, fun functions/2),
    RecordFields = new_record_fields(St3),
    NewRecord = new_new(St3),
    Readers = new_readers(St3#recmod.parameters,St3),
    ToParent = new_to_parent(St3),
    {Forms4, St4} = forms(Forms3, St3, fun exports/2),
    {H,T} = lists:splitwith(fun (X) ->
				    case X of
					{attribute,_,_,_} ->
					    true;
					_ -> 
					    false
				    end
			    end,     
			    Forms4),
    CForms = H ++ [RecordFields,NewRecord,ToParent] ++ Readers ++ T,
%    io:format("~p~n",[CForms]),
    CForms.
    

forms([F|Fs],St0,Fun) ->
    {F1,St1} = apply(Fun, [F,St0]),
    {Fs1,St} = forms(Fs,St1,Fun),
    {[F1|Fs1],St};
forms([], St0,_) ->
    {[], St0}.

%% form

% Get module name from -module(M)
form({attribute,L,module,M}=Form, St0) when is_atom(M) ->
    {Form, St0#recmod{ name=M }};
% TODO: do something when M is not an atom (original parametrized module, for example)
% form({attribute,L, module, M}=Form, St0) -> ?

% Get modrec fields
form({attribute,L,record,{Name, Fields}}=Form, #recmod{ name=Name }=St0) ->
    Params = fields(Fields),
    {Form, St0#recmod{ rec=Fields, parameters = Params }};

% Get static functions
form({attribute,L,static, StF}=Form, #recmod{ static=StF0 }=St0) when is_list(StF) ->
    {Form, St0#recmod{ static=(StF0 ++ StF) }};

% Get extends
form({attribute,L,extends, Extends}=Form, St0) when is_atom(Extends) ->
    {Form, St0#recmod{ extends=Extends }};

form(F, St0) ->
    {F, St0}.

%% functions

% Filter out non-static functions
functions({function,L,Name0,Arity0,Clauses0}=Form,#recmod{ static=StF, functions = Fs }=St0) ->
    case (lists:member(Name0, StF) orelse lists:member({Name0, Arity0}, StF)) of
	true -> % static function, skip it
	    {Form,St0};
	false -> % non-static function
	    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0, St0),
	    {{function,L,Name,Arity+1,Clauses},St0#recmod{ functions = [{Name0,Arity0}|Fs] } }
    end;

functions(F, St0) ->
    {F, St0}.

%% extension
extension({attribute,L,record,{Name, Fields}}=Form, #recmod{ extends=Name }=St0) ->
    Params = fields(Fields),
    {Form, St0#recmod{ extends_parameters = Params }};
extension(F, St0) ->
    {F, St0}.


%% exports
exports({attribute,L,export,Exports}, #recmod{static=StF,parameters=Params}=St) ->
    {{attribute,L,export, 
      lists:map(fun({Field,_Param}) ->
			{Field,1}
		end, Params) ++
      [{record_fields,0},{new,0},{to_parent,1}|
       lists:map(fun({Name0, Arity0}) ->
			 {Name0, Arity0+1}
		 end,
		 lists:filter(fun ({Name,Arity}) ->
				      not (lists:member(Name, StF) orelse lists:member({Name, Arity}, StF))
			     end,
			      Exports))]
     }, St};

exports(F, St0) ->
    {F, St0}.


%% record_fields
new_record_fields(St) ->
    {function,0,record_fields,0,
     [{clause, 0, [],[],
       [new_record_field(St#recmod.parameters)]}]}.

new_record_field([{F,_}|Fs]) ->
    {cons, 0, {atom, 0, F},
     new_record_field(Fs)};

new_record_field([]) ->
    {nil, 0}.

%% new
new_new(St) ->
    {function,0,new,0,
     [{clause, 0, [],[],
       [{record,0,St#recmod.name,[]}]}]}.


%% readers
new_readers([H|T], St) ->
    [new_reader(H,St)|new_readers(T,St)];
new_readers([],_) ->
    [].

new_reader({Field,Param},St) ->      
    {function,0,Field,1,
     [{clause,0,[{match, 0, {var, 0, '_'}, {var, 0, 'THIS'}}],[],
       [{record_field,0,{var,0,'THIS'},St#recmod.name,{atom, 0, Field}}]}]}.

%% to_parent

new_to_parent(#recmod{extends=Rec,extends_parameters=[_|_]}=St) when Rec /= undefined ->
    T = {tuple,0,[{atom, 0, St#recmod.name}|
		  [{var,0,list_to_atom("_" ++ atom_to_list(V))} || {_,V} <- St#recmod.parameters ]
		 ]},
    CommonFields = lists:map(fun ({F,_}) -> F end, sets:to_list(sets:intersection(sets:from_list(St#recmod.parameters),sets:from_list(St#recmod.extends_parameters)))),
    CommonRecFields = lists:map(fun (F) ->
					{record_field,0,{atom, 0, F},{record_field,0,{var,0,'THIS'},St#recmod.name,{atom,0,F}}}
				end, CommonFields),
    {function,0,to_parent,1,
     [{clause, 0, [{match, 0,T,{var,0,'THIS'}}], [],
       [{record, 0, Rec, CommonRecFields}]}]};

new_to_parent(St) ->    
    T = {tuple,0,[{atom, 0, St#recmod.name}|
		  [{var,0,list_to_atom("_" ++ atom_to_list(V))} || {_,V} <- St#recmod.parameters ]
		 ]},
    {function,0,to_parent,1,
     [{clause,0,[{match, 0, T,{var,0,'THIS'}}],[],
       [{var, 0, 'THIS'}]}]}.

%% fields
fields([F|Fs]) ->
    [field(F)|fields(Fs)];
fields([]) ->
    [].

field({record_field,_L,{atom, _L1, Field}}) ->
    {Field, parametrize(Field)};

field({record_field,_L,{atom, _L1, Field}, _InitExpr}) ->
    {Field, parametrize(Field)}.


%%
function(Name, Arity, Clauses0, St) ->
    Clauses1 = clauses(Name,Clauses0,St),
    {Name,Arity,Clauses1}.

clauses(Name,[C|Cs],St) ->
    {clause,L,H,G,B} = clause(C,St),
    T = {tuple,L,[{atom, L, St#recmod.name}|
		  [{var,L,list_to_atom("_" ++ atom_to_list(V))} || {_,V} <- St#recmod.parameters ]
		 ]},
    emit_clause(original, Name,L,H,T,G,B,St) ++ emit_clause(function_clause, Name,L,H,T,G,B,St) ++ emit_clause(coercion, Name,L,H,T,G,B,St) ++ clauses(Name,Cs,St);

clauses(_,[],_St) -> [].

clause({clause,L,H0,G0,B0},St) ->
    H1 = head(H0,St),
    G1 = guard(G0,St),
    B1 = exprs(B0,St),
    {clause,L,H1,G1,B1}.

emit_clause(original, Name, L,H,T,G,B,St) ->
    [{clause,L,H++[{match,L,T,{var,L,'THIS'}}],G,[{var,L,'THIS'}|B]}]; % original clause
emit_clause(function_clause, Name, L,H,T,G,B,#recmod{extends=Extends}=St) when Extends /= undefined ->
    {H1,_} = lists:foldl(fun (H0,{Hs,Ctr}) -> {[{match, L, H0, {var, L, list_to_atom("_Arg" ++ erlang:integer_to_list(Ctr))}}|Hs],Ctr+1} end, {[],1}, H),
    H1Args = lists:map(fun (Ctr) -> {var, L, list_to_atom("_Arg" ++ erlang:integer_to_list(Ctr))} end, lists:seq(1,length(H1))),
    [{clause,L,H1++[{match,L,T,{var,L,'THIS'}}],[], % function_clause "handler". Since it will most probably generate warnings, TODO: generate this conditionally
      [
       {call, L, {remote, L, {atom, L, St#recmod.extends}, {atom, L, Name}},
	H1Args ++ [{var, L, 'THIS'}]}
       
      ]}];
emit_clause(function_clause, _Name, _L,_H,_T,_G,_B,_St) ->
    [];
emit_clause(coercion, Name, L,H,T,G,B,St) ->
    [{clause,L,H++[{match,L,{var,L,'_'},{var,L,'THIS'}}],[], % THIS does not match, lets try to corce it
      [
       {call, L, {atom, L, Name}, [{call,L,{remote,L,{var,L,'THIS'},{atom, L, to_parent}}, []}]}
      ]}].


%%% Field name to parameter conversion
parametrize(FieldName) ->
    list_to_atom(inflector:camelize(atom_to_list(FieldName))).

%%% borrowed from sys_expand_pmod


head(Ps,St) -> patterns(Ps,St).

patterns([P0|Ps],St) ->
    P1 = pattern(P0,St),
    [P1|patterns(Ps,St)];
patterns([],_St) -> [].

string_to_conses([], _Line, Tail) ->
    Tail;
string_to_conses([E|Rest], Line, Tail) ->
    {cons, Line, {integer, Line, E}, string_to_conses(Rest, Line, Tail)}.

pattern({var,Line,V},_St) -> {var,Line,V};
pattern({match,Line,L0,R0},St) ->
    L1 = pattern(L0,St),
    R1 = pattern(R0,St),
    {match,Line,L1,R1};
pattern({integer,Line,I},_St) -> {integer,Line,I};
pattern({char,Line,C},_St) -> {char,Line,C};
pattern({float,Line,F},_St) -> {float,Line,F};
pattern({atom,Line,A},_St) -> {atom,Line,A};
pattern({string,Line,S},_St) -> {string,Line,S};
pattern({nil,Line},_St) -> {nil,Line};
pattern({cons,Line,H0,T0},St) ->
    H1 = pattern(H0,St),
    T1 = pattern(T0,St),
    {cons,Line,H1,T1};
pattern({tuple,Line,Ps0},St) ->
    Ps1 = pattern_list(Ps0,St),
    {tuple,Line,Ps1};
pattern({bin,Line,Fs},St) ->
    Fs2 = pattern_grp(Fs,St),
    {bin,Line,Fs2};
pattern({op,_Line,'++',{nil,_},R},St) ->
    pattern(R,St);
pattern({op,_Line,'++',{cons,Li,{char,C2,I},T},R},St) ->
    pattern({cons,Li,{char,C2,I},{op,Li,'++',T,R}},St);
pattern({op,_Line,'++',{cons,Li,{integer,L2,I},T},R},St) ->
    pattern({cons,Li,{integer,L2,I},{op,Li,'++',T,R}},St);
pattern({op,_Line,'++',{string,Li,L},R},St) ->
    pattern(string_to_conses(L, Li, R),St);
pattern({op,Line,Op,A},_St) ->
    {op,Line,Op,A};
pattern({op,Line,Op,L,R},_St) ->
    {op,Line,Op,L,R}.

pattern_grp([{bin_element,L1,E1,S1,T1} | Fs],St) ->
    S2 = case S1 of
	     default ->
		 default;
	     _ ->
		 expr(S1,St)
	 end,
    T2 = case T1 of
	     default ->
		 default;
	     _ ->
		 bit_types(T1)
	 end,
    [{bin_element,L1,expr(E1,St),S2,T2} | pattern_grp(Fs,St)];
pattern_grp([],_St) ->
    [].

bit_types([]) ->
    [];
bit_types([Atom | Rest]) when is_atom(Atom) ->
    [Atom | bit_types(Rest)];
bit_types([{Atom, Integer} | Rest]) when is_atom(Atom), is_integer(Integer) ->
    [{Atom, Integer} | bit_types(Rest)].

pattern_list([P0|Ps],St) ->
    P1 = pattern(P0,St),
    [P1|pattern_list(Ps,St)];
pattern_list([],_St) -> [].

guard([G0|Gs],St) when is_list(G0) ->
    [guard0(G0,St) | guard(Gs,St)];
guard(L,St) ->
    guard0(L,St).

guard0([G0|Gs],St) ->
    G1 = guard_test(G0,St),
    [G1|guard0(Gs,St)];
guard0([],_St) -> [].

guard_test(Expr={call,Line,{atom,La,F},As0},St) ->
    case erl_internal:type_test(F, length(As0)) of
	true ->
	    As1 = gexpr_list(As0,St),
	    {call,Line,{atom,La,F},As1};
	_ ->
	    gexpr(Expr,St)
    end;
guard_test(Any,St) ->
    gexpr(Any,St).

gexpr({var,L,V},St) ->
     case index(lists:keyfind(V,2,St#recmod.parameters),St#recmod.parameters) of
 	N when N > 0 ->
 	    {call,L,{remote,L,{atom,L,erlang},{atom,L,element}},
 	     [{integer,L,N+1},{var,L,'THIS'}]};
 	_ ->
 	    {var,L,V}
     end;
gexpr({integer,Line,I},_St) -> {integer,Line,I};
gexpr({char,Line,C},_St) -> {char,Line,C};
gexpr({float,Line,F},_St) -> {float,Line,F};
gexpr({atom,Line,A},_St) -> {atom,Line,A};
gexpr({string,Line,S},_St) -> {string,Line,S};
gexpr({nil,Line},_St) -> {nil,Line};
gexpr({cons,Line,H0,T0},St) ->
    H1 = gexpr(H0,St),
    T1 = gexpr(T0,St),
    {cons,Line,H1,T1};
gexpr({tuple,Line,Es0},St) ->
    Es1 = gexpr_list(Es0,St),
    {tuple,Line,Es1};
gexpr({call,Line,{atom,La,F},As0},St) ->
    case erl_internal:guard_bif(F, length(As0)) of
	true -> As1 = gexpr_list(As0,St),
		{call,Line,{atom,La,F},As1}
    end;
% Pre-expansion generated calls to erlang:is_record/3 must also be handled
gexpr({call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,is_record}},As0},St)
  when length(As0) =:= 3 ->
    As1 = gexpr_list(As0,St),
    {call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,is_record}},As1};
% Guard bif's can be remote, but only in the module erlang...
gexpr({call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As0},St) ->
    case erl_internal:guard_bif(F, length(As0)) or
	 erl_internal:arith_op(F, length(As0)) or 
	 erl_internal:comp_op(F, length(As0)) or
	 erl_internal:bool_op(F, length(As0)) of
	true -> As1 = gexpr_list(As0,St),
		{call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As1}
    end;
% Unfortunately, writing calls as {M,F}(...) is also allowed.
gexpr({call,Line,{tuple,La,[{atom,Lb,erlang},{atom,Lc,F}]},As0},St) ->
    case erl_internal:guard_bif(F, length(As0)) or
	 erl_internal:arith_op(F, length(As0)) or 
	 erl_internal:comp_op(F, length(As0)) or
	 erl_internal:bool_op(F, length(As0)) of
	true -> As1 = gexpr_list(As0,St),
		{call,Line,{tuple,La,[{atom,Lb,erlang},{atom,Lc,F}]},As1}
    end;
gexpr({bin,Line,Fs},St) ->
    Fs2 = pattern_grp(Fs,St),
    {bin,Line,Fs2};
gexpr({op,Line,Op,A0},St) ->
    case erl_internal:arith_op(Op, 1) orelse
	erl_internal:bool_op(Op, 1) of
	true -> A1 = gexpr(A0,St),
		{op,Line,Op,A1}
    end;
gexpr({op,Line,Op,L0,R0},St) ->
    case Op =:= 'andalso' orelse Op =:= 'orelse' orelse
	erl_internal:arith_op(Op, 2) orelse
	erl_internal:bool_op(Op, 2) orelse
	erl_internal:comp_op(Op, 2) of
	true ->
	    L1 = gexpr(L0,St),
	    R1 = gexpr(R0,St),
	    {op,Line,Op,L1,R1}
    end.

gexpr_list([E0|Es],St) ->
    E1 = gexpr(E0,St),
    [E1|gexpr_list(Es,St)];
gexpr_list([],_St) -> [].

exprs([E0|Es],St) ->
    E1 = expr(E0,St),
    [E1|exprs(Es,St)];
exprs([],_St) -> [].

expr({var,L,V},St) ->
     case index(lists:keyfind(V,2,St#recmod.parameters),St#recmod.parameters) of
 	N when N > 0 ->
 	    {call,L,{remote,L,{atom,L,erlang},{atom,L,element}},
 	     [{integer,L,N+1},{var,L,'THIS'}]};
 	_ ->
 	    {var,L,V}
     end;
expr({integer,Line,I},_St) -> {integer,Line,I};
expr({float,Line,F},_St) -> {float,Line,F};
expr({atom,Line,A},_St) -> {atom,Line,A};
expr({string,Line,S},_St) -> {string,Line,S};
expr({char,Line,C},_St) -> {char,Line,C};
expr({nil,Line},_St) -> {nil,Line};
expr({cons,Line,H0,T0},St) ->
    H1 = expr(H0,St),
    T1 = expr(T0,St),
    {cons,Line,H1,T1};
expr({lc,Line,E0,Qs0},St) ->
    Qs1 = lc_quals(Qs0,St),
    E1 = expr(E0,St),
    {lc,Line,E1,Qs1};
expr({tuple,Line,Es0},St) ->
    Es1 = expr_list(Es0,St),
    {tuple,Line,Es1};
expr({block,Line,Es0},St) ->
    Es1 = exprs(Es0,St),
    {block,Line,Es1};
expr({'if',Line,Cs0},St) ->
    Cs1 = icr_clauses(Cs0,St),
    {'if',Line,Cs1};
expr({'case',Line,E0,Cs0},St) ->
    E1 = expr(E0,St),
    Cs1 = icr_clauses(Cs0,St),
    {'case',Line,E1,Cs1};
expr({'receive',Line,Cs0},St) ->
    Cs1 = icr_clauses(Cs0,St),
    {'receive',Line,Cs1};
expr({'receive',Line,Cs0,To0,ToEs0},St) ->
    To1 = expr(To0,St),
    ToEs1 = exprs(ToEs0,St),
    Cs1 = icr_clauses(Cs0,St),
    {'receive',Line,Cs1,To1,ToEs1};
expr({'try',Line,Es0,Scs0,Ccs0,As0},St) ->
    Es1 = exprs(Es0,St),
    Scs1 = icr_clauses(Scs0,St),
    Ccs1 = icr_clauses(Ccs0,St),
    As1 = exprs(As0,St),
    {'try',Line,Es1,Scs1,Ccs1,As1};
expr({'fun',Line,Body,Info},St) ->
    case Body of
	{clauses,Cs0} ->
	    Cs1 = fun_clauses(Cs0,St),
	    {'fun',Line,{clauses,Cs1},Info};
	{function,F,A} ->
	    {F1,A1} = update_function_name({F,A},St),
	    if A1 == A ->
		    {'fun',Line,{function,F,A},Info};
	       true ->
		    %% Must rewrite local fun-name to a fun that does a
		    %% call with the extra THIS parameter.
		    As = make_vars(A, Line),
		    As1 = As ++ [{var,Line,'THIS'}],
		    Call = {call,Line,{atom,Line,F1},As1},
		    Cs = [{clause,Line,As,[],[Call]}],
		    {'fun',Line,{clauses,Cs},Info}
	    end;
	{function,M,F,A} ->			%This is an error in lint!
	    {'fun',Line,{function,M,F,A},Info}
    end;

expr({call,Lc,{atom,Lf,F}=Name,As0},#recmod{ static = StF}=St) ->
    case (lists:member(F, StF) orelse lists:member({F, length(As0)}, StF)) of
	true ->
	    As1 = expr_list(As0,St),
	    {call,Lc,Name,As1};
	false ->
	    %% Local function call - needs THIS parameter.
	    As1 = expr_list(As0,St),
	    {call,Lc,{atom,Lf,F},As1 ++ [{var,0,'THIS'}]}
    end;
expr({call,Line,F0,As0},St) ->
    %% Other function call
    F1 = expr(F0,St),
    As1 = expr_list(As0,St),
    {call,Line,F1,As1};
expr({'catch',Line,E0},St) ->
    E1 = expr(E0,St),
    {'catch',Line,E1};
expr({match,Line,P0,E0},St) ->
    E1 = expr(E0,St),
    P1 = pattern(P0,St),
    {match,Line,P1,E1};
expr({bin,Line,Fs},St) ->
    Fs2 = pattern_grp(Fs,St),
    {bin,Line,Fs2};
expr({op,Line,Op,A0},St) ->
    A1 = expr(A0,St),
    {op,Line,Op,A1};
expr({op,Line,Op,L0,R0},St) ->
    L1 = expr(L0,St),
    R1 = expr(R0,St),
    {op,Line,Op,L1,R1};
%% The following are not allowed to occur anywhere!
expr({remote,Line,M0,F0},St) ->
    M1 = expr(M0,St),
    F1 = expr(F0,St),
    {remote,Line,M1,F1};
expr(Rest,_St) ->
    Rest.

expr_list([E0|Es],St) ->
    E1 = expr(E0,St),
    [E1|expr_list(Es,St)];
expr_list([],_St) -> [].

icr_clauses([C0|Cs],St) ->
    C1 = clause(C0,St),
    [C1|icr_clauses(Cs,St)];
icr_clauses([],_St) -> [].

lc_quals([{generate,Line,P0,E0}|Qs],St) ->
    E1 = expr(E0,St),
    P1 = pattern(P0,St),
    [{generate,Line,P1,E1}|lc_quals(Qs,St)];
lc_quals([E0|Qs],St) ->
    E1 = expr(E0,St),
    [E1|lc_quals(Qs,St)];
lc_quals([],_St) -> [].

fun_clauses([C0|Cs],St) ->
    C1 = clause(C0,St),
    [C1|fun_clauses(Cs,St)];
fun_clauses([],_St) -> [].

% %% Return index from 1 upwards, or 0 if not in the list.
%
index(false, _) -> 0;
index(X,Ys) -> index(X,Ys,1).

index(X,[X|Ys],A) -> A;
index(X,[Y|Ys],A) -> index(X,Ys,A+1);
index(X,[],A) -> 0.

make_vars(N, L) ->
    make_vars(1, N, L).

make_vars(N, M, L) when N =< M ->
    V = list_to_atom("X"++integer_to_list(N)),
    [{var,L,V} | make_vars(N + 1, M, L)];
make_vars(_, _, _) ->
    [].


update_function_name(E, _St) ->
    E.
