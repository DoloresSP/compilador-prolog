tokenize_file(File, Tokens) :-
    open(File, read, F),read_string(F, "","",S, String), close(F), string_chars(String,T), 
	limpiartokens(T,SL), quitarrepes(SL,SL2),atomic_list_concat(SL2,StringL), split_string(StringL, "\n\n", "\t", Tokens).

limpiartokens([],[]).
limpiartokens([/|Rest],['\n',/,'\n'|Zy]):-limpiartokens(Rest,Zy).
limpiartokens([+|Rest],['\n',+,'\n'|Zy]):-limpiartokens(Rest,Zy).
limpiartokens([-|Rest],['\n',-,'\n'|Zy]):-limpiartokens(Rest,Zy).
limpiartokens([*|Rest],['\n',*,'\n'|Zy]):-limpiartokens(Rest,Zy).

limpiartokens([>,=|Rest],['\n','>=','\n'|Zy]):-limpiartokens(Rest,Zy).
limpiartokens([<,=|Rest],['\n','<=','\n'|Zy]):-limpiartokens(Rest,Zy).
limpiartokens([<|Rest],['\n',<,'\n'|Zy]):-limpiartokens(Rest,Zy).
limpiartokens([>|Rest],['\n',>,'\n'|Zy]):-limpiartokens(Rest,Zy).
limpiartokens([=|Rest],['\n',=,'\n'|Zy]):-limpiartokens(Rest,Zy).
limpiartokens([!,=|Rest],['\n','!=','\n'|Zy]):-limpiartokens(Rest,Zy).

limpiartokens([;|Rest],['\n',;,'\n'|Zy]):-limpiartokens(Rest,Zy).
limpiartokens([:,=|Rest],['\n',:=,'\n'|Zy]):-limpiartokens(Rest,Zy).
limpiartokens([:|Rest],['\n',:,'\n'|Zy]):-limpiartokens(Rest,Zy).
limpiartokens([.|Rest],['\n',.|Zy]):-limpiartokens(Rest,Zy).

limpiartokens([' '|Rest],['\n'|Zy]):-limpiartokens(Rest,Zy).

limpiartokens([X|Rest],[X|Zy]):- limpiartokens(Rest,Zy),!.

quitarrepes([],[]).
quitarrepes(['\n','\n'|Rest],['\n'|Zy]):-quitarrepes(Rest,Zy).
quitarrepes([X|Rest],[X|Zy]):-quitarrepes(Rest,Zy),!.

%PARSER

pl_program(D,E) --> ["program"], identifier(X), [";"], bloque(D,E), ["."].

bloque(D,E) --> declaracion(D), statement(E).

declaracion([V,X|R]) --> ["var"], variable(V,X), resto_variables(R).

resto_variables([V,X|R]) -->  [";"], variable(V,X), resto_variables(R).
resto_variables([]) --> [";"]. 

variable(V,X) --> identifier(V), [":"], ["Integer"], {X is 0}. 

statement((S;Ss)) --> ["begin"], statement(S), rest_statement(Ss).
statement(read(X)) --> ["read"], identifier(X), [";"].
statement(assign(X,V)) --> identifier(X), [":="], expression(V), [";"].
statement(if(T,S1,S2)) --> ["if"], test(T), ["then"], statement(S1), ["else"], statement(S2).
statement(while(T,S)) --> ["while"], test(T), ["do"], statement(S).

statement(write(X)) --> ["write"], expression(X), [";"].

rest_statement(end) --> ["end"].
rest_statement((S;Ss)) --> statement(S), rest_statement(Ss).

expression(X) --> pl_constant(X).
expression(expr(Op, X, Y)) --> pl_constant(X), aritmetic_op(Op), expression(Y).

aritmetic_op('+') --> ["+"].
aritmetic_op('*') --> ["*"].
aritmetic_op('-') --> ["-"].
aritmetic_op('/') --> ["/"].

pl_constant(X) --> identifier(X).

identifier(X) --> [X].

test(compare(Op,X,Y)) --> expression(X), comparison_op(Op), expression(Y).

comparison_op('=') --> ["="].
comparison_op('!=') --> ["!="].
comparison_op('>') --> [">"].
comparison_op('<') --> ["<"].
comparison_op('>=') --> [">="].
comparison_op('<=') --> ["<="].

%EJECUCION %%MAZO %%LOKA

ejecutar((S;Ss), V, NNV) :- ejecutar(S, V, NV), ejecutar(Ss, NV, NNV).
ejecutar(read(X),V, NV) :-  read(F), asignarvalor(X,F,V, NV).
ejecutar(assign(X,Y), V, NV) :- calcularExpresion(Y, V, S), asignarvalor(X, S, V, NV).
ejecutar(if(T,S1,S2),V, NV) :- comprobar(T, V),!, ejecutar(S1, V, NV).
ejecutar(if(T,S1,S2),V, NV) :- ejecutar(S2, V, NV).
ejecutar(while(T,S),V, NNV) :- comprobar(T, V),!, ejecutar(S, V, NV), ejecutar(while(T,S),NV,NNV).
ejecutar(while(T,S),NV,NV).
ejecutar(write(X),V, V) :- escribir(X, V).
ejecutar(end, V, V).

escribir(X,V) :- buscarValor(X, V, R1), write(X), write("="), write(R1), write("\n").
escribir(X,V) :- atom_number(X, R1), write_ln(R1).

calcularExpresion(expr(X, R, Z), V, S) :- atom_number(R, R1), atom_number(Z, Z1), operar(X, R1, Z1, S). 
calcularExpresion(expr(X, R, Z), V, S) :- atom_number(R, R1), buscarValor(Z, V, Z1), operar(X, R1, Z1, S).
calcularExpresion(expr(X, R, Z), V, S) :- buscarValor(R, V, R1), atom_number(Z, Z1), operar(X, R1, Z1, S).
calcularExpresion(expr(X, R, Z), V, S) :- buscarValor(R, V, R1), buscarValor(Z, V, Z1), operar(X, R1, Z1, S).
calcularExpresion(Y, V, S) :- atom_number(Y, S). 

operar(X, R1, Z1, S) :- X='+', S is R1+Z1.
operar(X, R1, Z1, S) :- X='*', S is R1*Z1.
operar(X, R1, Z1, S) :- X='-', S is R1-Z1.
operar(X, R1, Z1, S) :- X='/', S is R1/Z1.


comprobar(compare(X,Y,Z), V) :- X = '=', buscarValor(Y, V, Yi), buscarValor(Z, V, Zi), Yi=Zi.
comprobar(compare(X,Y,Z), V) :- X = '>', buscarValor(Y, V, Yi), buscarValor(Z, V, Zi), Yi>Zi.
comprobar(compare(X,Y,Z), V) :- X = '<', buscarValor(Y, V, Yi), buscarValor(Z, V, Zi), Yi<Zi.
comprobar(compare(X,Y,Z), V) :- X = '<=', buscarValor(Y, V, Yi), buscarValor(Z, V, Zi), Yi=<Zi.
comprobar(compare(X,Y,Z), V) :- X = '>=', buscarValor(Y, V, Yi), buscarValor(Z, V, Zi), Yi>=Zi.

buscarValor(Y, [X,S|R], Z) :- Y=X, Z=S.
buscarValor(Y, [X,S|R], Z) :- Y\=X, buscarValor(Y, R, Z).
buscarValor(Y, [], Z) :- fail.


asignarvalor(S, Val, [X,Y|R], V):- S==X , V=[S,Val|R].
asignarvalor(S, Val, [X,Y|R], V):- S\=X, asignarvalor(S, Val, R, E), V=[X,Y|E].
asignarvalor(S, Val, [], V):- fail.





parse(Source, Structure, Variables) :- pl_program(Structure, Variables, Source,[]), !.
analisiLexico(Ar,Ltok):- tokenize_file(Ar, Ltok).
compilar(Ar):- analisiLexico(Ar,L),parse(L,V, S), ejecutar(S, V, Fin), write("Estado: "), write_ln(Fin), !.
