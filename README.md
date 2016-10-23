## CS312 Project 1: Prolog Advisor
Prolog advisor can answer questions about Computer Science courses offered in the 2016/17W school year.

Use `ask([Q])` to ask yes/no questions. For example, `ask([is, pottinger, teaching, cpsc100]).`
Use `ask([Q],A)` to ask all other questions. For example, `ask([what,is,a,third,year,course],A).`

### Properties
+ Each course section is represented by an identifier. Eg. CPSC 100, section 101 has code cs100_101.
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

### Interesting feature to be implemented
+ Implement a student database.
+ Implement a pre-req checker. eg. can sam take cpscXXX?

### Problems
+ Can we use set_of to reduce repeated answers? (FIXED)

### Some questions we can implement
+ what does poole teach?
+ what course is offered in term1?
+ is cpsc310 in term1?
+ who is teaching section 1 of cpsc103?
+ who is teaching what section of cpsc103?
+ what section of cs221 is wolfman teaching?