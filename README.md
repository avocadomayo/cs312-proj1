## CS312 Project 1: Prolog Advisor
Prolog advisor can answer questions about Computer Science courses offered in the 2016/17W school year.

### Properties
+ Each course section is represented by a code. Eg. CPSC 100, section 101 has code cs100_101.
+ Each course has the following properties:

Property | Description
--- | ---
code | course code in format cpscXXX
year | course year 1, 2, 3 or 4
section | course section
title | course title
term | term offered
credits | number of credits awarded
activity | activity type (lecture, laboratory, tutorial)
instructor | instructor name in format ???
has_tutorial | true if has tutorial
has_lab | true if has lab ???
days | day(s) of lecture
startTime | start time designated in 24h clock
endTime | end time designated in 24h clock
building | lecture building
room | lecture room

### To be discussed
+ How do we want to handle lab/tutorial sections?
+ How do we want to handle prof names? Last_first? [last, first]? Last only?
+ Pre-requisites?
+ Database of student. Can Sam take cpscXXX?
+ Pre-req checker

### Problems
+ Try running `ask([what,is,a,first,year,course,taught,by,allen],A). `
+ Why are there so many answers? Is there a way to reduce?

#### Some questions we can implement
+ what does poole teach?
+ what course is offered in term1?
+ is cpsc310 in term1?
+ who is teaching section 1 of cpsc103?
+ who is teaching what section of cpsc103?
