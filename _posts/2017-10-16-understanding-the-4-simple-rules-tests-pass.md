---
layout: post
title: 'Understanding the 4 Simple Rules: Tests Pass'
category: technical-development simple-rules
---

# Overview

This week I was assigned the reading, _Understanding The 4 Rules of Simple Design_ by, Corey Haines.  The following series of post are my thoughts on each of the rules and some notes on how Corey helps us understand them.

# Tests Pass

This past week I had begun to actually code out TicTacToe in Erlang.  I had a lot of questions on testing.

_How do I test console output?_
_Do I test console output?_
_Why aren't my test better?_
_Why am I so confused?_

But the one question I should have asked was-

_Do my test pass?_


I embarrassingly forgot that I had updated some of my code inbetween commits and my IPM (iteration planning meeting).  My mentors chuckled because it was a pretty amateur mistake to make.  Overall they weren't totally disappointed with the IPM but one of the main notes was that I should clearly have all test passing before a pull request and definitely during any demo. 

# Why is this important

1. As crafters, we want to know our code works.  Passing tests tell us that our code is working.
2. As consultants, we look for ways to please our clients and give them peace of mind that what we are building for them can be proven to work.  Passing tests tell our clients that we are building a strong product and moving ahead only when code works.
3. Passing tests prove we understand our application.  
4. Passing tests tell us we can move onto writing more test and thus, writing more code.
