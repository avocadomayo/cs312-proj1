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

% ask(Q,A) gives an answer A to question Q
ask(Q,A) :-
	ask_all(Q,S),
    extract_a(S,A).
% ask(Q) returns true or false to the question Q
ask(Q) :-
    question(Q,[],_).

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
| ?- ask([what,is,the,title,of,cpsc416],A).
| ?- ask([how,many,credits,is,cpsc103],A).
| ?- ask([what,course,is,taught,by,wolfman],A).
| ?- ask([what,is,a,fourth,year,course],A).
| ?- ask([what,is,a,third,year,course,taught,by,poole],A).
*/

%  The Database of Courses

prop(cs100_101, code, cpsc100).
prop(cs100_101, year, 1).
prop(cs100_101, section, 101).
prop(cs100_101, title, [computational, thinking]).
prop(cs100_101, term, 1).
prop(cs100_101, credits, 3).
prop(cs100_101, activity, lecture).
prop(cs100_101, instructor, pottinger).
prop(cs100_101, activity, lecture).

prop(cs103_101, code, cpsc103).
prop(cs103_101, year, 1).
prop(cs103_101, section, 101).
prop(cs103_101, title, [introduction, to, systematic, program, design]).
prop(cs103_101, term, 1).
prop(cs103_101, credits, 3).
prop(cs103_101, activity, lecture).
prop(cs103_101, instructor, allen).
prop(cs103_101, instructor, wolfman).
% This course has two instructors

prop(cs103_201, code, cpsc103).
prop(cs103_201, year, 1).
prop(cs103_201, section, 201).
prop(cs103_201, title, [introduction, to, systematic, program, design]).
prop(cs103_201, term, 2).
prop(cs103_201, credits, 3).
prop(cs103_201, activity, lecture).
prop(cs103_201, instructor, allen).

prop(cs110_101, code, cpsc110).
prop(cs110_101, year, 1).
prop(cs110_101, section, 101).
prop(cs110_101, title, [computation, programs, and, programming]).
prop(cs110_101, term, 1).
prop(cs110_101, credits, 4).
prop(cs110_101, activity, lecture).
prop(cs110_101, instructor, berg).

prop(cs110_102, code, cpsc110).
prop(cs110_102, year, 1).
prop(cs110_102, section, 102).
prop(cs110_102, title, [computation, programs, and, programming]).
prop(cs110_102, term, 1).
prop(cs110_102, credits, 4).
prop(cs110_102, activity, lecture).
prop(cs110_102, instructor, kiczales).

prop(cs110_103, code, cpsc110).
prop(cs110_103, year, 1).
prop(cs110_103, section, 103).
prop(cs110_103, title, [computation, programs, and, programming]).
prop(cs110_103, term, 1).
prop(cs110_103, credits, 4).
prop(cs110_103, activity, lecture).
prop(cs110_103, instructor, little).

prop(cs110_104, code, cpsc110).
prop(cs110_104, year, 1).
prop(cs110_104, section, 104).
prop(cs110_104, title, [computation, programs, and, programming]).
prop(cs110_104, term, 1).
prop(cs110_104, credits, 4).
prop(cs110_104, activity, lecture).
prop(cs110_104, instructor, estey).

prop(cs110_bcs, code, cpsc110).
prop(cs110_bcs, year, 1).
prop(cs110_bcs, section, bcs).
prop(cs110_bcs, title, [computation, programs, and, programming]).
prop(cs110_bcs, term, 1).
prop(cs110_bcs, credits, 4).
prop(cs110_bcs, activity, lecture).
prop(cs110_bcs, instructor, kiczales).

prop(cs110_201, code, cpsc110).
prop(cs110_201, year, 1).
prop(cs110_201, section, 201).
prop(cs110_201, title, [computation, programs, and, programming]).
prop(cs110_201, term, 2).
prop(cs110_201, credits, 4).
prop(cs110_201, activity, lecture).
prop(cs110_201, instructor, mitchell).

prop(cs110_202, code, cpsc110).
prop(cs110_202, year, 1).
prop(cs110_202, section, 202).
prop(cs110_202, title, [computation, programs, and, programming]).
prop(cs110_202, term, 2).
prop(cs110_202, credits, 4).
prop(cs110_202, activity, lecture).
prop(cs110_202, instructor, carter).

prop(cs110_203, code, cpsc110).
prop(cs110_203, year, 1).
prop(cs110_203, section, 203).
prop(cs110_203, title, [computation, programs, and, programming]).
prop(cs110_203, term, 2).
prop(cs110_203, credits, 4).
prop(cs110_203, activity, lecture).
prop(cs110_203, instructor, aiello).

prop(cs121_101, code, cpsc121).
prop(cs121_101, year, 1).
prop(cs121_101, section, 101).
prop(cs121_101, title, [models, of, computation]).
prop(cs121_101, term, 1).
prop(cs121_101, credits, 4).
prop(cs121_101, activity, lecture).
prop(cs121_101, instructor, gao).

prop(cs121_102, code, cpsc121).
prop(cs121_102, year, 1).
prop(cs121_102, section, 102).
prop(cs121_102, title, [models, of, computation]).
prop(cs121_102, term, 1).
prop(cs121_102, credits, 4).
prop(cs121_102, activity, lecture).
prop(cs121_102, instructor, belleville).

prop(cs121_103, code, cpsc121).
prop(cs121_103, year, 1).
prop(cs121_103, section, 103).
prop(cs121_103, title, [models, of, computation]).
prop(cs121_103, term, 1).
prop(cs121_103, credits, 4).
prop(cs121_103, activity, lecture).
prop(cs121_103, instructor, belleville).

prop(cs121_202, code, cpsc121).
prop(cs121_202, year, 1).
prop(cs121_202, section, 202).
prop(cs121_202, title, [models, of, computation]).
prop(cs121_202, term, 2).
prop(cs121_202, credits, 4).
prop(cs121_202, activity, lecture).
prop(cs121_202, instructor, gao).

prop(cs121_203, code, cpsc121).
prop(cs121_203, year, 1).
prop(cs121_203, section, 203).
prop(cs121_203, title, [models, of, computation]).
prop(cs121_203, term, 2).
prop(cs121_203, credits, 4).
prop(cs121_203, activity, lecture).
prop(cs121_203, instructor, wolfman).

prop(cs121_bcs, code, cpsc121).
prop(cs121_bcs, year, 1).
prop(cs121_bcs, section, bcs).
prop(cs121_bcs, title, [models, of, computation]).
prop(cs121_bcs, term, 2).
prop(cs121_bcs, credits, 4).
prop(cs121_bcs, activity, lecture).
prop(cs121_bcs, instructor, wolfman).

prop(cs213_203, code, cpsc213).
prop(cs213_203, year, 2).
prop(cs213_203, section, 203).
prop(cs213_203, title, [introduction, to, computer,systems]).
prop(cs213_203, term, 2).
prop(cs213_203, credits, 3).
prop(cs213_203, activity, lecture).
prop(cs213_203, instructor, wagner).

prop(cs221_101, code, cpsc221).
prop(cs221_101, year, 2).
prop(cs221_101, section, 101).
prop(cs221_101, title, [basic, algorithms, and, data, structures]).
prop(cs221_101, term, 1).
prop(cs221_101, credits, 4).
prop(cs221_101, activity, lecture).
prop(cs221_101, instructor, manuch).

prop(cs221_102, code, cpsc221).
prop(cs221_102, year, 2).
prop(cs221_102, section, 102).
prop(cs221_102, title, [basic, algorithms, and, data, structures]).
prop(cs221_102, term, 1).
prop(cs221_102, credits, 4).
prop(cs221_102, activity, lecture).
prop(cs221_102, instructor, evans).

prop(cs221_201, code, cpsc221).
prop(cs221_201, year, 2).
prop(cs221_201, section, 201).
prop(cs221_201, title, [basic, algorithms, and, data, structures]).
prop(cs221_201, term, 2).
prop(cs221_201, credits, 4).
prop(cs221_201, activity, lecture).
prop(cs221_201, instructor, knorr).

prop(cs221_202, code, cpsc221).
prop(cs221_202, year, 2).
prop(cs221_202, section, 202).
prop(cs221_202, title, [basic, algorithms, and, data, structures]).
prop(cs221_202, term, 2).
prop(cs221_202, credits, 4).
prop(cs221_202, activity, lecture).
prop(cs221_202, instructor, manuch).

prop(cs259_101, code, cpsc259).
prop(cs259_101, year, 2).
prop(cs259_101, section, 101).
prop(cs259_101, title, [data, structures, and, algorithms, for, electrical, engineers]).
prop(cs259_101, term, 1).
prop(cs259_101, credits, 4).
prop(cs259_101, activity, lecture).
prop(cs259_101, instructor, awad).

prop(cs261_201, code, cpsc261).
prop(cs261_201, year, 2).
prop(cs261_201, section, 201).
prop(cs261_201, title, [basics, of, computer, systems]).
prop(cs261_201, term, 2).
prop(cs261_201, credits, 4).
prop(cs261_201, activity, lecture).
prop(cs261_201, instructor, schroeder).

prop(cs213_101, code, cpsc213).
prop(cs213_101, year, 2).
prop(cs213_101, section, 101).
prop(cs213_101, title, [introduction, to, computer,systems]).
prop(cs213_101, term, 1).
prop(cs213_101, credits, 4).
prop(cs213_101, activity, lecture).
prop(cs213_101, instructor, awad).

prop(cs301_201, code, cpsc301).
prop(cs301_201, year, 3).
prop(cs301_201, section, 201).
prop(cs301_201, title, [computing, in, life, sciences]).
prop(cs301_201, term, 2).
prop(cs301_201, credits, 3).
prop(cs301_201, activity, lecture).
prop(cs301_201, instructor, dawson).

prop(cs302_101, code, cpsc302).
prop(cs302_101, year, 3).
prop(cs302_101, section, 101).
prop(cs302_101, title, [numerical, computation, for, algebraic, problems]).
prop(cs302_101, term, 1).
prop(cs302_101, credits, 3).
prop(cs302_101, activity, lecture).
prop(cs302_101, instructor, friedlander).

prop(cs303_201, code, cpsc303).
prop(cs303_201, year, 3).
prop(cs303_201, section, 201).
prop(cs303_201, title, [numerical, approximation, and, discretization]).
prop(cs303_201, term, 2).
prop(cs303_201, credits, 3).
prop(cs303_201, activity, lecture).
% Instructor for this course TBA

prop(cs304_101, code, cpsc304).
prop(cs304_101, year, 3).
prop(cs304_101, section, 101).
prop(cs304_101, title, [introduction, to, relational, databases]).
prop(cs304_101, term, 1).
prop(cs304_101, credits, 3).
prop(cs304_101, activity, lecture).
prop(cs304_101, instructor, imran).

prop(cs304_201, code, cpsc304).
prop(cs304_201, year, 3).
prop(cs304_201, section, 201).
prop(cs304_201, title, [introduction, to, relational, databases]).
prop(cs304_201, term, 2).
prop(cs304_201, credits, 3).
prop(cs304_201, activity, lecture).
prop(cs304_201, instructor, lakshmanan).

prop(cs310_101, code, cpsc310).
prop(cs310_101, year, 3).
prop(cs310_101, section, 101).
prop(cs310_101, title, [introduction, software, engineering]).
prop(cs310_101, term, 1).
prop(cs310_101, credits, 4).
prop(cs310_101, activity, lecture).
prop(cs310_101, instructor, holmes).

prop(cs310_201, code, cpsc310).
prop(cs310_201, year, 3).
prop(cs310_201, section, 201).
prop(cs310_201, title, [introduction, software, engineering]).
prop(cs310_201, term, 2).
prop(cs310_201, credits, 4).
prop(cs310_201, activity, lecture).
% Instructor for this course TBA

prop(cs311_101, code, cpsc311).
prop(cs311_101, year, 3).
prop(cs311_101, section, 101).
prop(cs311_101, title, [definition, of, programming, languages]).
prop(cs311_101, term, 1).
prop(cs311_101, credits, 3).
prop(cs311_101, activity, lecture).
prop(cs311_101, instructor, dunfield).

prop(cs312_101, code, cpsc312).
prop(cs312_101, year, 3).
prop(cs312_101, section, 101).
prop(cs312_101, title, [functional, and, logic, programming]).
prop(cs312_101, term, 1).
prop(cs312_101, credits, 3).
prop(cs312_101, activity, lecture).
prop(cs312_101, instructor, poole).

prop(cs313_101, code, cpsc313).
prop(cs313_101, year, 3).
prop(cs313_101, section, 101).
prop(cs313_101, title, [computer, hardware, and, operating, systems]).
prop(cs313_101, term, 1).
prop(cs313_101, credits, 3).
prop(cs313_101, activity, lecture).
prop(cs313_101, instructor, feeley).

prop(cs313_202, code, cpsc313).
prop(cs313_202, year, 3).
prop(cs313_202, section, 202).
prop(cs313_202, title, [computer, hardware, and, operating, systems]).
prop(cs313_202, term, 2).
prop(cs313_202, credits, 3).
prop(cs313_202, activity, lecture).
prop(cs313_202, instructor, acton).

prop(cs314_101, code, cpsc314).
prop(cs314_101, year, 3).
prop(cs314_101, section, 101).
prop(cs314_101, title, [computer, graphics]).
prop(cs314_101, term, 1).
prop(cs314_101, credits, 3).
prop(cs314_101, activity, lecture).
prop(cs314_101, instructor, sheffer).

prop(cs314_201, code, cpsc314).
prop(cs314_201, year, 3).
prop(cs314_201, section, 201).
prop(cs314_201, title, [computer, graphics]).
prop(cs314_201, term, 2).
prop(cs314_201, credits, 3).
prop(cs314_201, activity, lecture).
prop(cs314_201, instructor, vandepanne).

prop(cs317_101, code, cpsc317).
prop(cs317_101, year, 3).
prop(cs317_101, section, 101).
prop(cs317_101, title, [internet, computing]).
prop(cs317_101, term, 1).
prop(cs317_101, credits, 3).
prop(cs317_101, activity, lecture).
prop(cs317_101, instructor, acton).

prop(cs317_201, code, cpsc317).
prop(cs317_201, year, 3).
prop(cs317_201, section, 201).
prop(cs317_201, title, [internet, computing]).
prop(cs317_201, term, 2).
prop(cs317_201, credits, 3).
prop(cs317_201, activity, lecture).
prop(cs317_201, instructor, acton).

prop(cs319_201, code, cpsc319).
prop(cs319_201, year, 3).
prop(cs319_201, section, 201).
prop(cs319_201, title, [software, engineering, project]).
prop(cs319_201, term, 2).
prop(cs319_201, credits, 4).
prop(cs319_201, activity, lecture).
prop(cs319_201, instructor, jim).

prop(cs320_102, code, cpsc320).
prop(cs320_102, year, 3).
prop(cs320_102, section, 102).
prop(cs320_102, title, [intermediate, algorithm, design, and, analysis]).
prop(cs320_102, term, 1).
prop(cs320_102, credits, 3).
prop(cs320_102, activity, lecture).
prop(cs320_102, instructor, wolfman).

prop(cs320_201, code, cpsc320).
prop(cs320_201, year, 3).
prop(cs320_201, section, 201).
prop(cs320_201, title, [intermediate, algorithm, design, and, analysis]).
prop(cs320_201, term, 2).
prop(cs320_201, credits, 3).
prop(cs320_201, activity, lecture).
prop(cs320_201, instructor, wolfman).

prop(cs320_202, code, cpsc320).
prop(cs320_202, year, 3).
prop(cs320_202, section, 202).
prop(cs320_202, title, [intermediate, algorithm, design, and, analysis]).
prop(cs320_202, term, 2).
prop(cs320_202, credits, 3).
prop(cs320_202, activity, lecture).
prop(cs320_202, instructor, manuch).

prop(cs322_101, code, cpsc322).
prop(cs322_101, year, 3).
prop(cs322_101, section, 101).
prop(cs322_101, title, [introduction, to, artificial, intelligence]).
prop(cs322_101, term, 1).
prop(cs322_101, credits, 3).
prop(cs322_101, activity, lecture).
prop(cs322_101, instructor, oveisifordoei).

prop(cs322_201, code, cpsc322).
prop(cs322_201, year, 3).
prop(cs322_201, section, 201).
prop(cs322_201, title, [introduction, to, artificial, intelligence]).
prop(cs322_201, term, 2).
prop(cs322_201, credits, 3).
prop(cs322_201, activity, lecture).
prop(cs322_201, instructor, conati).

prop(cs340_101, code, cpsc340).
prop(cs340_101, year, 3).
prop(cs340_101, section, 101).
prop(cs340_101, title, [machine, learning, and, data, mining]).
prop(cs340_101, term, 1).
prop(cs340_101, credits, 3).
prop(cs340_101, activity, lecture).
prop(cs340_101, instructor, schmidt).

prop(cs340_201, code, cpsc340).
prop(cs340_201, year, 3).
prop(cs340_201, section, 201).
prop(cs340_201, title, [machine, learning, and, data, mining]).
prop(cs340_201, term, 2).
prop(cs340_201, credits, 3).
prop(cs340_201, activity, lecture).
prop(cs340_201, instructor, gelbart).

prop(cs344_101, code, cpsc344).
prop(cs344_101, year, 3).
prop(cs344_101, section, 101).
prop(cs344_101, title, [introduction, to, human, computer, interaction, methods]).
prop(cs344_101, term, 1).
prop(cs344_101, credits, 3).
prop(cs344_101, activity, lecture).
prop(cs344_101, instructor, maclean).

prop(cs404_101, code, cpsc404).
prop(cs404_101, year, 4).
prop(cs404_101, section, 101).
prop(cs404_101, title, [advanced, relational, databases]).
prop(cs404_101, term, 1).
prop(cs404_101, credits, 3).
prop(cs404_101, activity, lecture).
prop(cs404_101, instructor, knorr).

prop(cs406_101, code, cpsc406).
prop(cs406_101, year, 4).
prop(cs406_101, section, 101).
prop(cs406_101, title, [computational, optimization]).
prop(cs406_101, term, 1).
prop(cs406_101, credits, 3).
prop(cs406_101, activity, lecture).
prop(cs406_101, instructor, ascher).

prop(cs411_201, code, cpsc411).
prop(cs411_201, year, 4).
prop(cs411_201, section, 201).
prop(cs411_201, title, [introduction, to, compiler, construction]).
prop(cs411_201, term, 2).
prop(cs411_201, credits, 3).
prop(cs411_201, activity, lecture).
prop(cs411_201, instructor, garcia).

prop(cs415_101, code, cpsc415).
prop(cs415_101, year, 4).
prop(cs415_101, section, 101).
prop(cs415_101, title, [advanced, operating, systems]).
prop(cs415_101, term, 1).
prop(cs415_101, credits, 3).
prop(cs415_101, activity, lecture).
prop(cs415_101, instructor, acton).

prop(cs416_201, code, cpsc416).
prop(cs416_201, year, 4).
prop(cs416_201, section, 201).
prop(cs416_201, title, [distributed, systems]).
prop(cs416_201, term, 2).
prop(cs416_201, credits, 3).
prop(cs416_201, activity, lecture).
prop(cs416_201, instructor, beschastnikh).

prop(cs418_201, code, cpsc418).
prop(cs418_201, year, 4).
prop(cs418_201, section, 201).
prop(cs418_201, title, [parallel, computation]).
prop(cs418_201, term, 2).
prop(cs418_201, credits, 3).
prop(cs418_201, activity, lecture).
prop(cs418_201, instructor, greenstreet).

prop(cs420_201, code, cpsc420).
prop(cs420_201, year, 4).
prop(cs420_201, section, 201).
prop(cs420_201, title, [advanced, algorithms, design, and, analysis]).
prop(cs420_201, term, 2).
prop(cs420_201, credits, 3).
prop(cs420_201, activity, lecture).
prop(cs420_201, instructor, evans).

prop(cs421_101, code, cpsc421).
prop(cs421_101, year, 4).
prop(cs421_101, section, 101).
prop(cs421_101, title, [introduction, to, theory, of, computing]).
prop(cs421_101, term, 1).
prop(cs421_101, credits, 3).
prop(cs421_101, activity, lecture).
prop(cs421_101, instructor, harvey).

prop(cs422_101, code, cpsc422).
prop(cs422_101, year, 4).
prop(cs422_101, section, 101).
prop(cs422_101, title, [intelligent, systems]).
prop(cs422_101, term, 1).
prop(cs422_101, credits, 3).
prop(cs422_101, activity, lecture).
prop(cs422_101, instructor, carenini).

prop(cs425_201, code, cpsc425).
prop(cs425_201, year, 4).
prop(cs425_201, section, 201).
prop(cs425_201, title, [computer, vision]).
prop(cs425_201, term, 2).
prop(cs425_201, credits, 3).
prop(cs425_201, activity, lecture).
prop(cs425_201, instructor, little).

prop(cs426_201, code, cpsc426).
prop(cs426_201, year, 4).
prop(cs426_201, section, 201).
prop(cs426_201, title, [computer, animation]).
prop(cs426_201, term, 2).
prop(cs426_201, credits, 3).
prop(cs426_201, activity, lecture).
prop(cs426_201, instructor, vandepanne).

prop(cs430_101, code, cpsc430).
prop(cs430_101, year, 4).
prop(cs430_101, section, 101).
prop(cs430_101, title, [computers, and, society]).
prop(cs430_101, term, 1).
prop(cs430_101, credits, 3).
prop(cs430_101, activity, lecture).
prop(cs430_101, instructor, dawson).

prop(cs444_201, code, cpsc444).
prop(cs444_201, year, 4).
prop(cs444_201, section, 201).
prop(cs444_201, title, [advanced, methods, for, human, computer, interaction]).
prop(cs444_201, term, 2).
prop(cs444_201, credits, 3).
prop(cs444_201, activity, lecture).
prop(cs444_201, instructor, mcgrenere).

prop(cs445_201, code, cpsc445).
prop(cs445_201, year, 4).
prop(cs445_201, section, 201).
prop(cs445_201, title, [algorithms, in, bioinformatics]).
prop(cs445_201, term, 2).
prop(cs445_201, credits, 3).
prop(cs445_201, activity, lecture).
prop(cs445_201, instructor, hoos).