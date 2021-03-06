## CS312 Project 1: Prolog Advisor
Prolog advisor can answer questions about Computer Science courses offered in the 2016/17W school year.

Use `ask([Q])` to ask yes/no questions. For example, `ask([is, pottinger, teaching, cs100]).`
Use `ask([Q],A)` to ask for singular answers for questions. For example, `ask([what,is,a,third,year,course],A).`
Use `ask_all([Q],L)` to ask for a list of answers questions. For example, `ask_all([what,is,a,third,year,course],L).`

### Properties
+ Each course section is represented by an identifier. Eg. cs 100, section 101 has code cs100_101.
+ Each course has the following properties:

Property | Description
--- | ---
COURSES
year | course year 1, 2, 3 or 4
title | course title
credits | number of credits awarded
prereq | list of prerequisites for the course

COURSE OFFERINGS
code | course code in format csXXX
section | course section
term | term offered
activity | activity type (lecture, laboratory, tutorial)
instructor | instructor name in format ???
has_tutorial | true if has tutorial
has_lab | true if has lab ???
days | day(s) of lecture
startTime | start time designated in 24h clock
endTime | end time designated in 24h clock
building | lecture building
room | lecture room

STUDENTS
name | name of the student
completed | list of completed courses

### Interesting feature to be implemented
+ Implement a student database.
+ Implement a pre-req checker. eg. can sam take csXXX?

### Problems
+ Can we use set_of to reduce repeated answers? (FIXED)

### Some questions we can implement
+ what does poole teach?
+ what course is offered in term1?
+ is cs310 in term1?
+ who is teaching section 1 of cs103?
+ who is teaching what section of cs103?
+ what section of cs221 is wolfman teaching?