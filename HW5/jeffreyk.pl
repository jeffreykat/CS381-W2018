% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- female(x), parent(X,_).
isFather(X) :- male(X), parent(X,_).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Z) :- parent(X,Y), parent(Y,Z).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(Z,X), parent(Z,Y), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- male(X), sibling(X,Y).
sister(X,Y) :- female(X), sibling(X,Y).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- sibling(X,Z), married(Z,Y), X \= Y.
siblingInLaw(X,Y) :- married(X,Z), sibling(Z,Y), X \= Y.

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- female(X), sibling(X,Z), parent(Z,Y).
aunt(X,Y) :- female(X), married(X,Z), sibling(Z,A), parent(A,Y).
uncle(X,Y) :- male(X), sibling(X,Z), parent(Z,Y).
uncle(X,Y) :- male(X), married(X,Z), sibling(Z,A), parent(A,Y).

% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- child(X,Z), sibling(Z,S), parent(S,Y).

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).

% Extra credit: Define the predicate `related/2`.



%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.
cmd(add, [X,Y|Z], S) :- N is (X+Y), S = [N|Z].
cmd(lte, [X,Y|Z], S) :- N = (X =< Y ->W=t; W=f), call(N), S = [W|Z].
cmd(if(X,_), [t|T], S) :- prog(X, T, S).
cmd(if(_,Y), [f|T], S) :- prog(Y, T, S).
cmd(X, T, S) :- S = [X|T].

% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.
prog([], X, Y) :- Y = X.
prog([X|Y], S, Z) :- cmd(X, S, T), prog(Y, T, Z).

