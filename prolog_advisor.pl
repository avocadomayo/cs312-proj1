% Prolog representation of a grammar to build a query for a database
%  This is not meant to be polished or lingustically reasonable, but purely to show what can be done

% This is slightly expanded code of Figures 12.10 and 12.11 in Section 12.6.6 of
% Poole and Mackworth, Artificial Intelligence: foundations of
% computational agents, Cambridge, 2010.

% Copyright (c) David Poole and Alan Mackworth 2010. This program
% is released under GPL, version 3 or later; see http://www.gnu.org/licenses/gpl.html

% noun_phrase(T0,T4,Ind,C0,C4) is true if
%  T0 and T4 are list of words, such that
%        T4 is an ending of T0
%        the words in T0 before T4 (written T0-T4) form a noun phrase
%  Ind is the individual that the noun phrase is referring to
%  C0 and C4 are lists of relations such that
%        C0 is an ending of C4 and
%        the relations in C4-C0 give the constraints on Ind implied by the noun phrase
% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind) :-
    det(T0,T1,Ind),
    adjectives(T1,T2,Ind),
    noun(T2,T3,Ind),
    mp(T3,T4,Ind).

% Try:
%?- noun_phrase([a,tall,student],T1,I1).
%?- noun_phrase([a,math,course],T2,I2).
%?- noun_phrase([a,tall,student,enrolled,in,a,math,course],T3,I3).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constaints.
det([the | T],T,_).
det([a | T],T,_).
det([an | T],T,_).
det(T,T,_).

% Adjectives consist of a sequence of adjectives.
% The meaning of the arguments is the same as for noun_phrase
adjectives(T0,T2,Ind) :-
    adj(T0,T1,Ind),
    adjectives(T1,T2,Ind).
adjectives(T,T,_).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(T0,T2,I1) :-
    reln(T0,T1,I1,I2),
    noun_phrase(T1,T2,I2).
mp([that|T0],T2,I1) :-
    reln(T0,T1,I1,I2),
    noun_phrase(T1,T2,I2).
mp(T,T,_).

% DICTIONARY

% adj(T0,T1,Ind) is true if T0-T1 is an adjective that is true of Ind
adj([computer, science | T],T,Ind) :- dept(Ind,comp_sci).
% adj([math | T],T,Ind) :- dept(Ind,math).
% adj([female | T],T,Ind) :- female(Ind).

% noun(T0,T1,Ind) is true if T0-T1 is a noun that is true of Ind
noun([course | T],T,Ind) :- prop(_, code, Ind).
noun([instructor | T],T,Ind) :- prop(_, instructor, Ind).
noun([building | T],T,Ind) :- prop(_, building, Ind).
% The following are for proper nouns:
noun([Ind | T],T,Ind) :- prop(_, code, Ind).

% reln(T0,T1,I1,I2,R0,R1) is true if T0-T1 is a relation
%   that provides relations R1-R0 on individuals I1 and I2
% reln([enrolled, in | T],T,I1,I2) :- enrolled_in(I1,I2).
% reln([passed | T],T,I1,I2) :- passed(I1,I2).
reln([teaching | T],T,I1,I2) :- prop(X,code,I2), prop(X,instructor,I1).

% question(Question,QR,Ind) is true if Question-QR is true of Ind
question([is | T0],T2,Ind) :-
    noun_phrase(T0,T1,Ind),
    mp(T1,T2,Ind).
question([who,is | T0],T1,Ind) :-
    mp(T0,T1,Ind).
question([who,is | T0],T1,Ind) :-
    noun_phrase(T0,T1,Ind).
question([who,is | T0],T1,Ind) :-
    adjectives(T0,T1,Ind).
question([what | T0],T2,Ind) :-
    noun_phrase(T0,[is|T1],Ind),
    mp(T1,T2,Ind).
question([what | T0],T2,Ind) :-
    noun_phrase(T0,T1,Ind),
    mp(T1,T2,Ind).


% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A).


%  The Database of Facts to be Queried

prop(cs100_101, code, cpsc100).
prop(cs100_101, section, 101).
prop(cs100_101, title, [computational, thinking]).
prop(cs100_101, activity, lecture).
prop(cs100_101, instructor, pottinger).
prop(cs100_101, term, 1).
prop(cs100_101, credits, 3).
prop(cs100_101, activity, lecture).
prop(cs100_101, building, dmp).

/* Try the following queries
| ?- ask([who, is, teaching, cpsc100], A)
| ?- ask([who,is,an,instructor],A).
| ?- ask([is,john,enrolled,in,cs312],_)..
| ?- ask([who,is,tall],A).
| ?- ask([is,john,enrolled,in,a,computer,science,course],_).
| ?- ask([who,is,enrolled,in,a,computer,science,course],A).
| ?- ask([who,is,a,tall,student,enrolled,in,a,computer,science,course],A).
| ?- ask([what,student,is,enrolled,in,a,computer,science,course],A).
| ?- ask([what,student,passed,a,computer,science,course],A).
| ?- ask([what,student,enrolled,in,a,math,course,passed,a,computer,science,course],A).
| ?- ask([what,student,passed,a,computer,science,course,enrolled,in,a,math,course],A).
| ?- ask([what,student,passed,cs312],A).
*/