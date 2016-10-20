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
adj([first, year | T],T,Ind) :- prop(C, code, Ind), prop(C, year, 1).
adj([second, year | T],T,Ind) :- prop(C, code, Ind), prop(C, year, 2).
adj([third, year | T],T,Ind) :- prop(C, code, Ind), prop(C, year, 3).
adj([fourth, year | T],T,Ind) :- prop(C, code, Ind), prop(C, year, 4).

% noun(T0,T1,Ind) is true if T0-T1 is a noun that is true of Ind
noun([course | T],T,Ind) :- prop(_, code, Ind).
noun([term | T],T,Ind) :- prop(_, term, Ind).
noun([instructor | T],T,Ind) :- prop(_, instructor, Ind).
noun([credits | T],T,Ind) :- prop(_, credit, Ind).
% The following are for proper nouns:
noun([Ind | T],T,Ind) :- prop(_, code, Ind).
noun([Ind | T],T,Ind) :- prop(_, instructor, Ind).

% reln(T0,T1,I1,I2,R0,R1) is true if T0-T1 is a relation
%   that provides relations R1-R0 on individuals I1 and I2
% reln([enrolled, in | T],T,I1,I2) :- enrolled_in(I1,I2).
% reln([passed | T],T,I1,I2) :- passed(I1,I2).
reln([teaching | T],T,I1,I2) :- prop(C,code,I2), prop(C,instructor,I1).
reln([teaches | T],T,I1,I2) :- prop(C,code,I2), prop(C,instructor,I1).
reln([taught, by | T],T,I1,I2) :- prop(C,code,I1), prop(C,instructor,I2).
reln([term,is | T],T,I1,I2) :- prop(C,term,I1), prop(C,code,I2).
reln([the, title, of | T],T,I1,I2) :- prop(C,title,I1), prop(C,code,I2).
reln([credits, is | T],T,I1,I2) :- prop(C,credits,I1), prop(C,code,I2).

% question(Question,QR,Ind) is true if Question-QR is true of Ind

% handles: who is an instructor
question([is | T0],T2,Ind) :-
    noun_phrase(T0,T1,Ind),
    mp(T1,T2,Ind).
    
% handles: who is teaching
question([who,is | T0],T1,Ind) :-
    mp(T0,T1,Ind).

% handles: who is an instructor
question([who,is | T0],T1,Ind) :-
    noun_phrase(T0,T1,Ind).

% handles what is a first year course?
question([what,is | T0],T1,Ind) :-
    noun_phrase(T0,T1,Ind).

% handles: what term is XXX, what course is taught
question([what | T0],T2,Ind) :-
    noun_phrase(T0,[is|T1],Ind),
    mp(T1,T2,Ind).

% what term is XXX?
question([what | T0],T1,Ind) :-
    mp(T0,T1,Ind).

% handles what is the XXX of
question([what,is | T0],T1,Ind) :-
    mp(T0,T1,Ind).
    
% handles: how many credits is XXX
question([how,many | T0],T1,Ind) :-
    mp(T0,T1,Ind).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A).


%  The Database of Facts to be Queried

prop(cs100_101, code, cpsc100).
prop(cs100_101, year, 1).
prop(cs100_101, section, 101).
prop(cs100_101, title, [computational, thinking]).
prop(cs100_101, term, 1).
prop(cs100_101, credits, 3).
prop(cs100_101, activity, lecture).
prop(cs100_101, instructor, pottinger).
prop(cs100_101, activity, lecture).
prop(cs100_101, building, dmp).
prop(cs100_101, room, 110).

prop(cs103_101, code, cpsc103).
prop(cs103_101, year, 1).
prop(cs103_101, section, 101).
prop(cs103_101, title, [introduction, to, systematic, program, design]).
prop(cs103_101, term, 1).
prop(cs103_101, credits, 3).
prop(cs103_101, activity, lecture).
prop(cs103_101, instructor, allen).
prop(cs103_101, instructor, wolfman).
prop(cs103_101, building, dmp).


prop(cs103_201, code, cpsc103).
prop(cs103_201, year, 1).
prop(cs103_201, section, 201).
prop(cs103_201, title, [introduction, to, systematic, program, design]).
prop(cs103_201, term, 2).
prop(cs103_201, credits, 3).
prop(cs103_201, activity, lecture).
prop(cs103_201, instructor, allen).

prop(cs213_203, code, cpsc213).
prop(cs213_203, year, 2).
prop(cs213_203, section, 203).
prop(cs213_203, title, [introduction, to, computer,systems]).
prop(cs213_203, term, 2).
prop(cs213_203, credits, 3).
prop(cs213_203, activity, lecture).
prop(cs213_203, instructor, wagner).

prop(cs312_101, code, cpsc312).
prop(cs312_101, year, 3).
prop(cs312_101, section, 101).
prop(cs312_101, title, [functional, and, logic, programming]).
prop(cs312_101, term, 1).
prop(cs312_101, credits, 3).
prop(cs312_101, activity, lecture).
prop(cs312_101, instructor, poole).

prop(cs213_101, code, cpsc213).
prop(cs213_101, year, 2).
prop(cs213_101, section, 101).
prop(cs213_101, title, [introduction, to, computer,systems]).
prop(cs213_101, term, 1).
prop(cs213_101, credits, 3).
prop(cs213_101, activity, lecture).
prop(cs213_101, instructor, awad).

/* Try the following queries
| ?- ask([who, is, teaching, cpsc100], A).
| ?- ask([who, is, an, instructor, that, teaches, cpsc103], A).
| ?- ask([who, is, an, instructor],A).
| ?- ask([what, term, is, cpsc103], A).
| ?- ask([is, pottinger, teaching, cpsc100],A).
| ?- ask([what,is,the,title,of,cpsc213],A).
| ?- ask([how,many,credits,is,cpsc103],A).
| ?- ask([what,course,is,taught,by,wolfman],A).
| ?- ask([what,is,a,third,year,course],A).
| ?- ask([what,is,a,third,year,course,taught,by,poole],A).
*/