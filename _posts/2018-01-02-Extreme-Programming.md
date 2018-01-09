---
layout: post
title:  Extreme Programming
date:   2017-01-02 14:48:09 -0500
categories: Agile
---

In order to increase the effectiveness of this blog and shorten the time I 
spend writing it.  I'm going to list each of the [Practices of Extreme Programming](http://wiki.c2.com/?ExtremeProgrammingCorePractices)
as described in [PPP](https://www.amazon.com/Software-Development-Principles-Patterns-Practices/dp/0135974445), along with my short thoughts on each specific practice.  
<hr>

###  Customer Team Member
To me this means...
The people we are building software for should have as much equity on a team as the developers. It makes no sense to build products that satisfy the coder more than the consumer, so working closely with the entities for which we build features is paramount to building features that actually matter.
<hr>

### User Stories
To me this means...
Requirements should be easily stated, and should be focused on creating value for users, and not focused on the details of a feature's implementation.  In XP this is done through assessing each individual user story's priority and importance with the customer.  

<hr>
### Short Cycles
To me this means...
That we make an effort to minimize the time between deliveries of code while maintaining a steady pace. 
<blockquote>
a. The Iteration Plan
b. The Release Plan
</blockquote>
a. The Iteration Plan describes a usually, 2 week long set of specific feature implementations.  
b. The Release Plan is more general and describes a major release of a software project, usually encapsulating some 3 months worth of iterations. 
<hr>

### Acceptance Tests
To me this means...
A set of test should be written such that the details of a feature are described and automatically tested at each release or integration.

<hr>
### Pair Programming
To me this means...
Two programmers author code together usually with one keyboard and one screen, passing the _navigation_ responsibility and the _driving_ responsibility back and forth throughout the process. This improves code and decreases the defect rate. 

<hr>
### Test-Driven Development
To me this means... 
Implementation occurs only through the writing of a failing test and then the writing of code to make that test pass.  Red, Green, Refactor.  

<hr>
### Collective Ownership
To me this means...
Anyone can check out and improve any module in a code base. No one has any special rights or responsibility in developing and maintaining a specific domain or section in a program.  

<hr>
### Continuous Integration
To me this means... 
That we are constantly pushing up code that's ready for review. The first pair to push to the _source of truth_ repo wins, and everyone else then merges those changes into their remote branches.  

<hr>
### Sustainable Pace
To me this means... 
An effort towards a balanced schedule is made, overtime is acceptable only in the last iteration of a release.  _'It's a marathon, not a sprint.'_

<hr>
### Open Workspace
To me this means...
Teams of developers work in an open setting where everyone is within ear shot of one another. An effort should be made to create an _operations center_ style environment. 

<hr>
### The Planning Game
To me this means... 
A game is played between the business focued team members and developers, whereby developers create a budget of time they expect a set of features to consume based upon previous iterations, and then the business team members are given a chance to allocate that budget how they see fit. 

<hr>
### Simple Design
In XP this is described through these three principles:
<blockquote>
a. Do the simplest thing that could work.  
b. You aren't going to need it.
c. Once and only once.
</blockquote>
To me this means... 
Write small code, for a specific problem, and don't repeat yourself. 

<hr>
### Refactoring
To me this means...
At each and every change, an effort should be made to organize/re-organize your code so that it's the implemented in the most sensical and effective way.  

<hr>
### Metaphor
To me this means...
Create a useful and meaningful set of language tools that describe your programs major pieces with coiled concision.   
<hr>