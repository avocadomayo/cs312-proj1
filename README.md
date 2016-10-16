## CS312 Project 1: Prolog Advisor
Prolog advisor can answer questions about Computer Science courses offered in the 2016/17W school year.

### Properties
+ Each course section is represented by a code. Eg. CPSC 100, section 101 has code cs100_101.
+ Each course has the following properties:

Property | Description
--- | ---
code | course code in format cpscXXX
section | course section
title | course title
term | term offered
credits | number of credits awarded
activity | activity type (lecture, laboratory, tutorial)
instructor | instructor name in format ???
days | day(s) of lecture
startTime | start time designated in 24h clock
endTime | end time designated in 24h clock
building | lecture building
room | lecture room

### To be discussed
+ How do we want to handle lab/tutorial sections?
+ How do we want to handle prof names? Last_first? [last, first]? Last only?
+ Question example: is poole teaching cs312?