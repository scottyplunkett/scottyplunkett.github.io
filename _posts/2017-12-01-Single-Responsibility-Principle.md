---
layout: post
title:  Single-Responsibility-Principle
date:   2017-12-01 14:48:09 -0500
categories: technical-development SOLID
---

As humans we are constantly juggling responsiblities. It's very easy to get tangled up and torn between different roles and the folks that depend on us to fill them. On a nearly daily basis there'll be reasons to change something in my work schedule because of something I need to do in my personal life. There is little we can do about that, our nature and social being neccessitates taking on more than one responsibility at a time. Life gets messy where these tensions exist.  In our code however, we have control.  We are the designers of the society in which our application lives and works and we ought to help our software by delegating particular behaviors to correspondingly defined components, keeping the related efforts together.  This is what Tom Demarco and Meilir Page-Jones called cohesion, or the functional relatedness of the elements of a module. Developing highly cohesive modules/classes/etc. is the purpose of the _Single Responsibility Principle_.

<blockquote>A class should have only one reason to change.</blockquote>


So how do we control the scope of responsbilities in our code?  When I first learned this concept, I thought it meant: **separate everything**. That led to modules and classes where even related methods are torn apart from their logical homes. Or at least sometimes... I fundamentally didn't understand the driving force behind **_why_** seperations should exist and this led to code where in parts I would go through, finding the smallest software entities and dividing them trivially, while in other parts I would somehow find an excuse or reason to not apply the same refinement. So that when I presented my Elixir GoL it was filled with [*needless complexity*](https://github.com/scottyplunkett/GameOfLife/tree/6d826c5cb57aef27ef63a7101a360c231799ead0/lib). When asked for further explanation on the form of my application, and why there was so much going on, I began trying to walk through my code by explaining its complex model, which was not easy.

Why did I make my code more complicated than it has to be?  It's because I didn't know how to view the application as an axis of change. That is to say, we chunk our code according to the application requirements, when requirements change, we manifest them through changes in the responsbilities of our classes, where no need for change exist, no change is made. This is an  important vantage point to take. Most new devs think that the single responsibility principle means we should have one method per class, but this causes an insanely time consuming, wasteful, and convoluted implementation of a program's natural order, especially when you are building an app that is young and doesn't need more than a few methods.  We make changes as changes occur, not before they occur.  [*That brought my GoL to this much more understandable point*](https://github.com/scottyplunkett/GameOfLife/tree/master/lib)

Sometimes you do know ahead of time where you'll need to define seperate modules. Store vs. There is one area in particular where as we code it should become clear that some kind of seperation must exist becuase of their different frequencies of change and behavior. A violation of the SRP occurs inside a class where both persisting parts, and algorithmic parts are kept. The algorithmic parts should reflect the business rules, and will likely change w/ high frequency.  The persisting parts rarely require change but when they do it is always for a different reason than the rules, it's job is to store things and storing things is at least conceptually different than whatever block is applied to create the things we need to store.

So there are a couple of important things to remember when evaluating if your code upholds the SRP.
- We make changes only where and when they are required, and only if those changes actually occur.
- Responsibilities exist along axises of change.  Business rules and structures for Persistence change differently, so they should be separated.
Maybe you can find some ways to apply these rules to your personal life as well and find some benefit or stress reduction in applying the SRP when and where your life demands change.

Further Reading:
See what Uncle Bob had to say about [*The Single Responsibility Principle*](https://8thlight.com/blog/uncle-bob/2014/05/08/SingleReponsibilityPrinciple.html) in this article he wrote for [*8th Light's Blog*](https://8thlight.com/blog/).

<sub style="color:gray">
Test:
<ul style="color:lightgray">
<li>Does the intro have an attn getter? Yes
<li>Does the intro describe and define the topic covered at a high lvl? Yes
<li>Does this have some major points each mapped to a single paragraph? Yes
<li>Does the body have one coding example? Yes
<li>Does the conclusion summarize the information presented? Yes
<li>Does the conclusion include at least one further reading link? Yes
</ul>
</sub>
