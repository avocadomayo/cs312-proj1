## CS312 Project 1: Prolog Advisor
Prolog advisor can answer questions about Computer Science courses offered in the 2016/17W school year.

### Properties
+ Each course section is represented by a code. Eg. CPSC 100, section 101 has code cs100_101.
+ Each course has the following properties:

Property | Description
--- | ---
course | course code in format CPSCXXX
title | course title
section | course section
activity | activity type (lecture, laboratory, tutorial)
term | term offered
days | day(s) of lecture
startTime | start time designated in 24h clock
endTime | end time designated in 24h clock
instructor | instructor name in format last, first
credits | number of credits awarded
building | lecture building
room | lecture room

### To be discussed
+ How do we want to handle lab/tutorial sections?