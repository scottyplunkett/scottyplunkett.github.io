---
layout: post
title: The Facade Pattern
date:  2018-01-16 14:48:09 -0500
category: patterns
---

The facade pattern is kind of dubious... Like a scandalous politician, it hides away the ugly details of a story and only exposes to the public a controlled _prettified_ simple and specific interface. This of course is not as dubious a design pattern as the [mediator pattern](https://en.wikipedia.org/wiki/Mediator_pattern), but it is still an attempt at hiding away the ugliness of more utilitarian objects behind a more consumable abstraction.

In [PPP](https://www.amazon.com/Software-Development-Principles-Patterns-Practices/dp/0135974445), Martin introduces the concept of the facade pattern with an example of a DB Facade.
This made me immediately think of Rail's MVC architecture and more specifically, ActiveRecord, which is the ORM (object-relational mapping) between Rails and a Rail's apps database. What happens with ActiveRecord is we're given a set of policies that essentially set a convention for a public interactions with our data. This facade is big and visible and meant to ensure that our data objects are not operated on directly.

It should be noted that not all Rails apps use the ActiveRecord ORM, however I can tell you from a recent project where a mixture of using the ActiveRecord facade and actual SQL calls was implemented, that picking a method for data operations and transaction, one way or the other, is crucial to the long term success of a team project. Going way or the other and sticking to it as a group will make things easier in the long run. Plus in XP, aren't we supposed to let anyone on the team checkout any module?  Shouldn't they know what they are supposed to do in the case they check out that module without going through a bunch of rigamarole?
