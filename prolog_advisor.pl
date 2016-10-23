% Prolog representation of a grammar to build a query for a database

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
adj([first, year | T],T,Ind) :- prop(Ind, year, 1).
adj([second, year | T],T,Ind) :- prop(Ind, year, 2).
adj([third, year | T],T,Ind) :- prop(Ind, year, 3).
adj([fourth, year | T],T,Ind) :- prop(Ind, year, 4).

% noun(T0,T1,Ind) is true if T0-T1 is a noun that is true of Ind
noun([course | T],T,Ind) :- prop(_, code, Ind).
noun([term | T],T,Ind) :- prop(_, term, Ind).
noun([instructor | T],T,Ind) :- prop(_, instructor, Ind).
noun([credits | T],T,Ind) :- prop(_, credit, Ind).
% The following are for proper nouns:
noun([Ind | T],T,Ind) :- prop(_, code, Ind).
noun([Ind | T],T,Ind) :- prop(_, instructor, Ind).
noun([Ind | T],T,Ind) :- prop(_, name, Ind).
noun([Ind | T],T,Ind) :- prop(Ind, _, _).

% reln(T0,T1,I1,I2,R0,R1) is true if T0-T1 is a relation
%   that provides relations R1-R0 on individuals I1 and I2
reln([teaching | T],T,I1,I2) :- prop(I2,instructor,I1).
reln([teaching | T],T,I1,I2) :- prop(C,code,I2), prop(C,instructor,I1).
reln([teaches | T],T,I1,I2) :- prop(I2,instructor,I1).
reln([teaches | T],T,I1,I2) :- prop(C,code,I2), prop(C,instructor,I1).
reln([taught, by | T],T,I1,I2) :- prop(C,code,I1), prop(C,instructor,I2).
reln([term,is | T],T,I1,I2) :- prop(I2,term,I1).
reln([term,is | T],T,I1,I2) :- prop(C,term,I1), prop(C,code,I2).
reln([the, title, of | T],T,I1,I2) :- prop(I2,code,C), prop(C,title,I1).
reln([the, title, of | T],T,I1,I2) :- prop(I2,title,I1).
reln([credits, is | T],T,I1,I2) :- prop(I2,code,C), prop(C,credits,I1).
reln([credits, is | T],T,I1,I2) :- prop(I2,credits,I1).
reln([take | T],T,I1,I2) :-
    prop(I2,code,C), prop(S,name,I1),
    prop(S,completed,CC), prop(C,prereq,PC),
    subset(PC,CC).
reln([take | T],T,I1,I2) :-
    prop(C,code,I2), prop(_,name,I1), \+prop(C,prereq,_).
reln([take | T],T,I1,I2) :-
    prop(S,name,I1),
    prop(S,completed,CC), prop(I2,prereq,PC),
    subset(PC,CC).
reln([take | T],T,I1,I2) :-
    prop(_,name,I1), \+prop(I2,prereq,_).

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

% handles: can {name} take {course}
question([can | T0],T2,Ind) :-
    noun_phrase(T0,T1,Ind),
    mp(T1,T2,Ind).

% ask(Q,A) gives an answer A to question Q
ask(Q,A) :-
	ask_all(Q,S),
    extract_a(S,A).
% ask(Q) returns true or false to the question Q
ask(Q) :-
    setof(V, question(Q,[],V),_).

% ask_all(Q) returns a set of all answers A for Q
ask_all(Q, A) :-
    setof(V, question(Q,[],V),A).

% extracts an answer A from the set of set of answers
extract_a([A | _],A).
extract_a([_ | T],A) :-
	extract_a(T,A).

/* Try the following queries
| ?- ask([who,is,teaching,cpsc100], A).
| ?- ask([who,is,an,instructor,that,teaches,cpsc121], A).
| ?- ask([who,is,an,instructor],A).
| ?- ask([what,term,is,cpsc320], A).
| ?- ask([is,pottinger,teaching,cpsc100]).
| ?- ask([is,pottinger,teaching,cs100_101]).
| ?- ask([what,is,the,title,of,cpsc416],A).
| ?- ask([what,is,the,title,of,cs416_201],A).
| ?- ask([how,many,credits,is,cpsc103],A).
| ?- ask([how,many,credits,is,cs103_101],A).
| ?- ask([what,course,is,taught,by,wolfman],A).
| ?- ask([what,is,a,fourth,year,course],A).
| ?- ask([what,is,a,third,year,course,taught,by,poole],A).

| ?- ask([can, sam, take, cpsc110],A).
*/

%%% The Database of Courses

%CPSC100
prop(cpsc100, year, 1).
prop(cpsc100, title, [computational, thinking]).
prop(cpsc100, credits, 3).

prop(cs100_101, code, cpsc100).
prop(cs100_101, section, 101).
prop(cs100_101, term, 1).
prop(cs100_101, activity, lecture).
prop(cs100_101, instructor, pottinger).

%CPSC103
prop(cpsc103, year, 1).
prop(cpsc103, title, [introduction, to, systematic, program, design]).
prop(cpsc103, credits, 3).

prop(cs103_101, code, cpsc103).
prop(cs103_101, section, 101).
prop(cs103_101, term, 1).
prop(cs103_101, activity, lecture).
prop(cs103_101, instructor, allen).
prop(cs103_101, instructor, wolfman).
% This course has two instructors

prop(cs103_201, code, cpsc103).
prop(cs103_201, section, 201).
prop(cs103_201, term, 2).
prop(cs103_201, activity, lecture).
prop(cs103_201, instructor, allen).

%CPSC110
prop(cpsc110, year, 1).
prop(cpsc110, title, [computation, programs, and, programming]).
prop(cpsc110, credits, 4).

prop(cs110_101, code, cpsc110).
prop(cs110_101, section, 101).
prop(cs110_101, term, 1).
prop(cs110_101, activity, lecture).
prop(cs110_101, instructor, berg).

prop(cs110_102, code, cpsc110).
prop(cs110_102, section, 102).
prop(cs110_102, term, 1).
prop(cs110_102, activity, lecture).
prop(cs110_102, instructor, kiczales).

prop(cs110_103, code, cpsc110).
prop(cs110_103, section, 103).
prop(cs110_103, term, 1).
prop(cs110_103, activity, lecture).
prop(cs110_103, instructor, little).

prop(cs110_104, code, cpsc110).
prop(cs110_104, section, 104).
prop(cs110_104, term, 1).
prop(cs110_104, activity, lecture).
prop(cs110_104, instructor, estey).

prop(cs110_bcs, code, cpsc110).
prop(cs110_bcs, section, bcs).
prop(cs110_bcs, term, 1).
prop(cs110_bcs, activity, lecture).
prop(cs110_bcs, instructor, kiczales).

prop(cs110_201, code, cpsc110).
prop(cs110_201, section, 201).
prop(cs110_201, term, 2).
prop(cs110_201, activity, lecture).
prop(cs110_201, instructor, mitchell).

prop(cs110_202, code, cpsc110).
prop(cs110_202, section, 202).
prop(cs110_202, term, 2).
prop(cs110_202, activity, lecture).
prop(cs110_202, instructor, carter).

prop(cs110_203, code, cpsc110).
prop(cs110_203, section, 203).
prop(cs110_203, term, 2).
prop(cs110_203, activity, lecture).
prop(cs110_203, instructor, aiello).

%CPSC121
prop(cpsc121, year, 1).
prop(cpsc121, title, [models, of, computation]).
prop(cpsc121, credits, 4).

prop(cs121_101, code, cpsc121).
prop(cs121_101, section, 101).
prop(cs121_101, term, 1).
prop(cs121_101, activity, lecture).
prop(cs121_101, instructor, gao).

prop(cs121_102, code, cpsc121).
prop(cs121_102, section, 102).
prop(cs121_102, term, 1).
prop(cs121_102, activity, lecture).
prop(cs121_102, instructor, belleville).

prop(cs121_103, code, cpsc121).
prop(cs121_103, section, 103).
prop(cs121_103, term, 1).
prop(cs121_103, activity, lecture).
prop(cs121_103, instructor, belleville).

prop(cs121_202, code, cpsc121).
prop(cs121_202, section, 202).
prop(cs121_202, term, 2).
prop(cs121_202, activity, lecture).
prop(cs121_202, instructor, gao).

prop(cs121_203, code, cpsc121).
prop(cs121_203, section, 203).
prop(cs121_203, term, 2).
prop(cs121_203, activity, lecture).
prop(cs121_203, instructor, wolfman).

prop(cs121_bcs, code, cpsc121).
prop(cs121_bcs, section, bcs).
prop(cs121_bcs, term, 2).
prop(cs121_bcs, activity, lecture).
prop(cs121_bcs, instructor, wolfman).

%cpsc210
prop(cpsc210, year, 2).
prop(cpsc210, title, [software, construction]).
prop(cpsc210, credits, 4).

prop(cs210_101, code, cpsc210).
prop(cs210_101, section, 101).
prop(cs210_101, term, 1).
prop(cs210_101, activity, lecture).
prop(cs210_101, instructor, hutchinson).
prop(cs210_101, prereq, [cpsc110]).
prop(cs210_101, prereq, [cpsc260]).

prop(cs210_102, code, cpsc210).
prop(cs210_102, section, 102).
prop(cs210_102, term, 1).
prop(cs210_102, activity, lecture).
prop(cs210_102, instructor, schroeder).
prop(cs210_102, prereq, [cpsc110]).
prop(cs210_102, prereq, [cpsc260]).

prop(cs210_103, code, cpsc210).
prop(cs210_103, section, 103).
prop(cs210_103, term, 1).
prop(cs210_103, activity, lecture).
prop(cs210_103, instructor, vogt).
prop(cs210_103, prereq, [cpsc110]).
prop(cs210_103, prereq, [cpsc260]).

prop(cs210_201, code, cpsc210).
prop(cs210_201, section, 201).
prop(cs210_201, term, 2).
prop(cs210_201, activity, lecture).
prop(cs210_201, instructor, hutchinson).
prop(cs210_201, prereq, [cpsc110]).
prop(cs210_201, prereq, [cpsc260]).

prop(cs210_202, code, cpsc210).
prop(cs210_202, section, 202).
prop(cs210_202, term, 2).
prop(cs210_202, activity, lecture).
prop(cs210_202, instructor, feeley).
prop(cs210_202, prereq, [cpsc110]).
prop(cs210_202, prereq, [cpsc260]).

prop(cs210_203, code, cpsc210).
prop(cs210_203, section, 203).
prop(cs210_203, term, 2).
prop(cs210_203, activity, lecture).
prop(cs210_203, instructor, carter).
prop(cs210_203, prereq, [cpsc110]).
prop(cs210_203, prereq, [cpsc260]).

prop(cs210_bcs, code, cpsc210).
prop(cs210_bcs, section, bcs).
prop(cs210_bcs, term, 2).
prop(cs210_bcs, activity, lecture).
prop(cs210_bcs, instructor, feeley).
prop(cs210_bcs, prereq, [cpsc110]).
prop(cs210_bcs, prereq, [cpsc260]).

%CPSC213
prop(cpsc213, year, 2).
prop(cpsc213, title, [introduction, to, computer, systems]).
prop(cpsc213, credits, 3).
prop(cpsc213, prereq, [cpsc121,cpsc210]).
prop(cpsc213, prereq, [cpsc210,cpsc260,eece256]).

prop(cs213_101, code, cpsc213).
prop(cs213_101, section, 101).
prop(cs213_101, term, 1).
prop(cs213_101, activity, lecture).
prop(cs213_101, instructor, awad).

prop(cs213_203, code, cpsc213).
prop(cs213_203, section, 203).
prop(cs213_203, term, 2).
prop(cs213_203, credits, 3).
prop(cs213_203, activity, lecture).
prop(cs213_203, instructor, wagner).

%cpsc221
prop(cpsc221, year, 2).
prop(cpsc221, title, [basic, algorithms, and, data, structures]).
prop(cpsc221, credits, 4).
prop(cpsc221, prereq, [cpsc210,cpsc121]).
prop(cpsc221, prereq, [cpsc210,math220]).
prop(cpsc221, prereq, [eece210,cpsc121]).
prop(cpsc221, prereq, [eece210,math220]).
prop(cpsc221, prereq, [cpsc121,cpsc121]).
prop(cpsc221, prereq, [cpsc121,math220]).


prop(cs221_101, code, cpsc221).
prop(cs221_101, section, 101).
prop(cs221_101, term, 1).
prop(cs221_101, activity, lecture).
prop(cs221_101, instructor, manuch).

prop(cs221_102, code, cpsc221).
prop(cs221_102, section, 102).
prop(cs221_102, term, 1).
prop(cs221_102, activity, lecture).
prop(cs221_102, instructor, evans).

prop(cs221_201, code, cpsc221).
prop(cs221_201, section, 201).
prop(cs221_201, term, 2).
prop(cs221_201, activity, lecture).
prop(cs221_201, instructor, knorr).

prop(cs221_202, code, cpsc221).
prop(cs221_202, section, 202).
prop(cs221_202, term, 2).
prop(cs221_202, activity, lecture).
prop(cs221_202, instructor, manuch).

%CPSC259
prop(cpsc259, year, 2).
prop(cpsc259, title, [data, structures, and, algorithms, for, electrical, engineers]).
prop(cpsc259, credits, 4).
prop(cpsc259, prereq, [apsc160]).

prop(cs259_101, code, cpsc259).
prop(cs259_101, section, 101).
prop(cs259_101, term, 1).
prop(cs259_101, activity, lecture).
prop(cs259_101, instructor, awad).

%CPSC261
prop(cpsc261, year, 2).
prop(cpsc261, title, [basics, of, computer, systems]).
prop(cpsc261, credits, 4).
prop(cpsc261, prereq, [eece259, cpen211]).
prop(cpsc261, prereq, [cpsc260, eece210, cpen221]).

prop(cs261_201, code, cpsc261).
prop(cs261_201, section, 201).
prop(cs261_201, term, 2).
prop(cs261_201, activity, lecture).
prop(cs261_201, instructor, schroeder).

%CPSC301
prop(cpsc301, year, 3).
prop(cpsc301, title, [computing, in, life, sciences]).
prop(cpsc301, credits, 3).
prop(cpsc301, prereq, [cpsc210]).

prop(cs301_201, code, cpsc301).
prop(cs301_201, section, 201).
prop(cs301_201, term, 2).
prop(cs301_201, activity, lecture).
prop(cs301_201, instructor, dawson).

%CPSC302
prop(cpsc302, year, 3).
prop(cpsc302, title, [numerical, computation, for, algebraic, problems]).
prop(cpsc302, credits, 3).

prop(cs302_101, code, cpsc302).
prop(cs302_101, section, 101).
prop(cs302_101, term, 1).
prop(cs302_101, activity, lecture).

%CPSC303
prop(cpsc303, year, 3).
prop(cpsc303, title, [numerical, approximation, and, discretization]).
prop(cpsc303, credits, 3).

prop(cs303_201, code, cpsc303).
prop(cs303_201, section, 201).
prop(cs303_201, term, 2).
prop(cs303_201, activity, lecture).
% Instructor for this course TBA

%CPSC304
prop(cpsc304, year, 3).
prop(cpsc304, title, [introduction, to, relational, databases]).
prop(cpsc304, credits, 3).
prop(cs304_101, prereq, [cpsc221]).
prop(cs304_101, prereq, [cpsc260,eece320,cpsc210]).
prop(cs304_101, prereq, [cpsc260,eece320,eece210]).
prop(cs304_101, prereq, [cpsc260,eece320,eece309]).

prop(cs304_101, code, cpsc304).
prop(cs304_101, section, 101).
prop(cs304_101, term, 1).
prop(cs304_101, activity, lecture).
prop(cs304_101, instructor, imran).

prop(cs304_201, code, cpsc304).
prop(cs304_201, section, 201).
prop(cs304_201, term, 2).
prop(cs304_201, activity, lecture).
prop(cs304_201, instructor, lakshmanan).

%CPSC310
prop(cpsc310, year, 3).
prop(cpsc310, title, [introduction, software, engineering]).
prop(cpsc310, credits, 4).
prop(cpsc310, prereq, [cpsc210]).

prop(cs310_101, code, cpsc310).
prop(cs310_101, section, 101).
prop(cs310_101, term, 1).
prop(cs310_101, activity, lecture).
prop(cs310_101, instructor, holmes).

prop(cs310_201, code, cpsc310).
prop(cs310_201, section, 201).
prop(cs310_201, term, 2).
prop(cs310_201, activity, lecture).
% Instructor for this course TBA

%CPSC311
prop(cpsc311, year, 3).
prop(cpsc311, title, [definition, of, programming, languages]).
prop(cpsc311, credits, 3).
prop(cpsc311, prereq, [cpsc210]).

prop(cs311_101, code, cpsc311).
prop(cs311_101, section, 101).
prop(cs311_101, term, 1).
prop(cs311_101, activity, lecture).
prop(cs311_101, instructor, dunfield).

%CPSC312
prop(cpsc312, year, 3).
prop(cpsc312, title, [functional, and, logic, programming]).
prop(cpsc312, credits, 3).
prop(cpsc312, prereq, [cpsc210]).
prop(cpsc312, prereq, [eece210]).
prop(cpsc312, prereq, [eece309]).
prop(cpsc312, prereq, [cpen221]).

prop(cs312_101, code, cpsc312).
prop(cs312_101, section, 101).
prop(cs312_101, term, 1).
prop(cs312_101, activity, lecture).
prop(cs312_101, instructor, poole).

%CPSC313
prop(cpsc313, year, 3).
prop(cpsc313, title, [computer, hardware, and, operating, systems]).
prop(cpsc313, credits, 3).
prop(cpsc313, prereq, [cpsc213,cpsc221]).
prop(cpsc313, prereq, [cpsc210,cpsc213,cpsc260,eece320]).

prop(cs313_101, code, cpsc313).
prop(cs313_101, section, 101).
prop(cs313_101, term, 1).
prop(cs313_101, activity, lecture).
prop(cs313_101, instructor, feeley).

prop(cs313_202, code, cpsc313).
prop(cs313_202, section, 202).
prop(cs313_202, term, 2).
prop(cs313_202, activity, lecture).
prop(cs313_202, instructor, acton).


%CPSC314
prop(cpsc314, year, 3).
prop(cpsc314, title, [computer, graphics]).
prop(cpsc314, credits, 3).

prop(cs314_101, code, cpsc314).
prop(cs314_101, section, 101).
prop(cs314_101, term, 1).
prop(cs314_101, activity, lecture).
prop(cs314_101, instructor, sheffer).

prop(cs314_201, code, cpsc314).
prop(cs314_201, section, 201).
prop(cs314_201, term, 2).
prop(cs314_201, activity, lecture).
prop(cs314_201, instructor, vandepanne).


%CPSC317
prop(cpsc317, year, 3).
prop(cpsc317, title, [internet, computing]).
prop(cpsc317, credits, 3).
prop(cpsc317, prereq, [cpsc213,cpsc221]).
prop(cpsc317, prereq, [cpsc213,cpsc210,cpsc260,eece320]).

prop(cs317_101, code, cpsc317).
prop(cs317_101, section, 101).
prop(cs317_101, term, 1).
prop(cs317_101, activity, lecture).
prop(cs317_101, instructor, acton).

prop(cs317_201, code, cpsc317).
prop(cs317_201, section, 201).
prop(cs317_201, term, 2).
prop(cs317_201, activity, lecture).
prop(cs317_201, instructor, acton).

%CPSC319
prop(cpsc319, year, 3).
prop(cpsc319, title, [software, engineering, project]).
prop(cpsc319, credits, 4).
prop(cpsc319, prereq, [cpsc310]).

prop(cs319_201, code, cpsc319).
prop(cs319_201, section, 201).
prop(cs319_201, term, 2).
prop(cs319_201, activity, lecture).
prop(cs319_201, instructor, jim).

%CPSC320
prop(cpsc320, year, 3).
prop(cpsc320, title, [intermediate, algorithm, design, and, analysis]).
prop(cpsc320, credits, 3).
prop(cpsc320, prereq, [cpsc221]).
prop(cpsc320, prereq, [cpsc260,eece320]).

prop(cs320_102, code, cpsc320).
prop(cs320_102, section, 102).
prop(cs320_102, term, 1).
prop(cs320_102, activity, lecture).
prop(cs320_102, instructor, wolfman).

prop(cs320_201, code, cpsc320).
prop(cs320_201, section, 201).
prop(cs320_201, term, 2).
prop(cs320_201, activity, lecture).
prop(cs320_201, instructor, wolfman).

prop(cs320_202, code, cpsc320).
prop(cs320_202, section, 202).
prop(cs320_202, term, 2).
prop(cs320_202, activity, lecture).
prop(cs320_202, instructor, manuch).

%CPSC322
prop(cpsc322, year, 3).
prop(cpsc322, title, [introduction, to, artificial, intelligence]).
prop(cpsc322, credits, 3).
prop(cpsc322, prereq, [cpsc221]).
prop(cpsc322, prereq, [cpsc260,eece320,cpsc210]).
prop(cpsc322, prereq, [cpsc260,eece320,eece210]).
prop(cpsc322, prereq, [cpsc260,eece320,eece309]).

prop(cs322_101, code, cpsc322).
prop(cs322_101, section, 101).
prop(cs322_101, term, 1).
prop(cs322_101, activity, lecture).
prop(cs322_101, instructor, oveisifordoei).

prop(cs322_201, code, cpsc322).
prop(cs322_201, section, 201).
prop(cs322_201, term, 2).
prop(cs322_201, activity, lecture).
prop(cs322_201, instructor, conati).

%CPSC340
prop(cpsc340, year, 3).
prop(cpsc340, title, [machine, learning, and, data, mining]).
prop(cpsc340, credits, 3).

prop(cs340_101, code, cpsc340).
prop(cs340_101, section, 101).
prop(cs340_101, term, 1).
prop(cs340_101, activity, lecture).
prop(cs340_101, instructor, schmidt).

prop(cs340_201, code, cpsc340).
prop(cs340_201, section, 201).
prop(cs340_201, term, 2).
prop(cs340_201, activity, lecture).
prop(cs340_201, instructor, gelbart).

%CPSC344
prop(cpsc344, year, 3).
prop(cpsc344, title, [introduction, to, human, computer, interaction, methods]).
prop(cpsc344, credits, 3).
prop(cpsc344, prereq, [cpsc210]).
prop(cpsc344, prereq, [eece210]).
prop(cpsc344, prereq, [eece309]).
prop(cpsc344, prereq, [cpen221]).

prop(cs344_101, code, cpsc344).
prop(cs344_101, section, 101).
prop(cs344_101, term, 1).
prop(cs344_101, activity, lecture).
prop(cs344_101, instructor, maclean).

%CPSC404
prop(cpsc404, year, 4).
prop(cpsc404, title, [advanced, relational, databases]).
prop(cpsc404, credits, 3).
prop(cpsc404, prereq, [cpsc304,cpsc213]).
prop(cpsc404, prereq, [cpsc304,cpsc261]).

prop(cs404_101, code, cpsc404).
prop(cs404_101, section, 101).
prop(cs404_101, term, 1).
prop(cs404_101, activity, lecture).
prop(cs404_101, instructor, knorr).

%CPSC406
prop(cpsc406, year, 4).
prop(cpsc406, title, [computational, optimization]).
prop(cpsc406, credits, 3).
prop(cpsc406, prereq, [cpsc302]).
prop(cpsc406, prereq, [cpsc303]).
prop(cpsc406, prereq, [math307]).


prop(cs406_101, code, cpsc406).
prop(cs406_101, section, 101).
prop(cs406_101, term, 1).
prop(cs406_101, activity, lecture).
prop(cs406_101, instructor, ascher).

%CPSC411
prop(cpsc411, year, 4).
prop(cpsc411, title, [introduction, to, compiler, construction]).
prop(cpsc411, credits, 3).
prop(cpsc411, prereq, [cpsc311,cpsc213]).

prop(cs411_201, code, cpsc411).
prop(cs411_201, section, 201).
prop(cs411_201, term, 2).
prop(cs411_201, activity, lecture).
prop(cs411_201, instructor, garcia).

%CPSC415
prop(cpsc415, year, 4).
prop(cpsc415, title, [advanced, operating, systems]).
prop(cpsc415, credits, 3).
prop(cpsc415, prereq, [cpsc313]).
prop(cpsc415, prereq, [eece315]).
prop(cpsc415, prereq, [cpen331]).

prop(cs415_101, code, cpsc415).
prop(cs415_101, section, 101).
prop(cs415_101, term, 1).
prop(cs415_101, activity, lecture).
prop(cs415_101, instructor, acton).

%CPSC416
prop(cpsc416, year, 4).
prop(cpsc416, title, [distributed, systems]).
prop(cpsc416, credits, 3).
prop(cpsc416, prereq, [cpsc313,cpsc317]).
prop(cpsc416, prereq, [cpsc313,eece358]).
prop(cpsc416, prereq, [cpsc313,elec331]).
prop(cpsc416, prereq, [eece315,cpsc317]).
prop(cpsc416, prereq, [eece315,eece358]).
prop(cpsc416, prereq, [eece315,elec331]).
prop(cpsc416, prereq, [cpen331,cpsc317]).
prop(cpsc416, prereq, [cpen331,eece358]).
prop(cpsc416, prereq, [cpen331,elec331]).

prop(cs416_201, code, cpsc416).
prop(cs416_201, section, 201).
prop(cs416_201, term, 2).
prop(cs416_201, activity, lecture).
prop(cs416_201, instructor, beschastnikh).

%CPSC418
prop(cpsc418, year, 4).
prop(cpsc418, title, [parallel, computation]).
prop(cpsc418, credits, 3).

prop(cs418_201, code, cpsc418).
prop(cs418_201, section, 201).
prop(cs418_201, term, 2).
prop(cs418_201, activity, lecture).
prop(cs418_201, instructor, greenstreet).

%CPSC420
prop(cpsc420, year, 4).
prop(cpsc420, title, [advanced, algorithms, design, and, analysis]).
prop(cpsc420, credits, 3).
prop(cpsc420, prereq, [cpsc320]).

prop(cs420_201, code, cpsc420).
prop(cs420_201, section, 201).
prop(cs420_201, term, 2).
prop(cs420_201, activity, lecture).
prop(cs420_201, instructor, evans).

%CPSC421
prop(cpsc421, year, 4).
prop(cpsc421, title, [introduction, to, theory, of, computing]).
prop(cpsc421, credits, 3).
prop(cpsc421, prereq, [cpsc221]).
prop(cpsc421, prereq, [cpsc260,eece320]).

prop(cs421_101, code, cpsc421).
prop(cs421_101, section, 101).
prop(cs421_101, term, 1).
prop(cs421_101, activity, lecture).
prop(cs421_101, instructor, harvey).

%CPSC422
prop(cpsc422, year, 4).
prop(cpsc422, title, [intelligent, systems]).
prop(cpsc422, credits, 3).
prop(cpsc422, prereq, [cpsc322]).

prop(cs422_101, code, cpsc422).
prop(cs422_101, section, 101).
prop(cs422_101, term, 1).
prop(cs422_101, activity, lecture).
prop(cs422_101, instructor, carenini).

%CPSC425
prop(cpsc425, year, 4).
prop(cpsc425, title, [computer, vision]).
prop(cpsc425, credits, 3).
prop(cpsc425, prereq, [math200,math221,cpsc221]).
prop(cpsc425, prereq, [math200,math221,cpsc260,eece320]).

prop(cs425_201, code, cpsc425).
prop(cs425_201, section, 201).
prop(cs425_201, term, 2).
prop(cs425_201, activity, lecture).
prop(cs425_201, instructor, little).

%CPSC426
prop(cpsc426, year, 4).
prop(cpsc426, title, [computer, animation]).
prop(cpsc426, credits, 3).
prop(cpsc426, prereq, [cpsc314,eece478]).

prop(cs426_201, code, cpsc426).
prop(cs426_201, section, 201).
prop(cs426_201, term, 2).
prop(cs426_201, activity, lecture).
prop(cs426_201, instructor, vandepanne).

%CPSC430
prop(cpsc430, year, 4).
prop(cpsc430, title, [computers, and, society]).
prop(cpsc430, credits, 3).

prop(cs430_101, code, cpsc430).
prop(cs430_101, section, 101).
prop(cs430_101, term, 1).
prop(cs430_101, activity, lecture).
prop(cs430_101, instructor, dawson).


%CPSC444
prop(cpsc444, year, 4).
prop(cpsc444, title, [advanced, methods, for, human, computer, interaction]).
prop(cpsc444, credits, 3).
prop(cpsc444, prereq, [cpsc310,cpsc344,stat200]).
prop(cpsc444, prereq, [cpsc310,cpsc344,stat241]).

prop(cs444_201, code, cpsc444).
prop(cs444_201, section, 201).
prop(cs444_201, term, 2).
prop(cs444_201, activity, lecture).
prop(cs444_201, instructor, mcgrenere).

%CPSC445
prop(cpsc444, year, 4).
prop(cpsc444, title, [algorithms, in, bioinformatics]).
prop(cpsc444, credits, 3).
prop(cpsc444, prereq, [cpsc320]).

prop(cs445_201, code, cpsc445).
prop(cs445_201, section, 201).
prop(cs445_201, term, 2).
prop(cs445_201, activity, lecture).
prop(cs445_201, instructor, hoos).

% Mock Database of Students
prop(sn11111111, name, sam).
prop(sn11111111, completed, []).

prop(sn22222222, name, tammy).
prop(sn22222222, completed, [cpsc110]).

prop(sn33333333, name, ruth).
prop(sn33333333, completed, [cpsc110,cpsc121,cpsc210,cpsc213,cpsc221,cpsc310]).

prop(sn44444444, name, wendy).
prop(sn44444444, completed, [cpen221]).