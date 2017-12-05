---
layout: post
title:  Open-Closed Principle
date:   2017-12-01 14:48:09 -0500
categories: technical-development SOLID
---

Think about the day you were born. Not much you were capable of doing. A squirming little bundle of joy completely helpless in the face of a dangerous dynamic world... You are for all intents and purposes the same creature as you were that day, same DNA, same bloodtype, fingerprints, etc. So how did you make it this far? You extended you're ability through new behaviors, not new fundamental forms. That process is one that we hope to mimic in our code because it follows the Open-Closed Principle.

<blockquote>
Software Entities (classes, modules, functions, etc.) should be open for extension, but closed for modification.
</blockquote>

We use abstractions where they are neccessary. By initially writing code expecting it not to change we can quickly get to a defensible and coherent first pass. From there we can ease in system changes as they occur, extending the generic functionality with detailed implementations. Through this approach we can create new behaviors without altering the source files already in place. Modules should be closed off by depending on fixed abstractions, and from those fixed abstractions we can create new dirivatives.  This means we literally don't have to touch a closed off module once it's linked to an abstraction.  It's openness comes from the fact that associated behaviors are unbound.

It's important to consider that our first iteration through coding an application is probably going to be needlessly complex if we separate our modules too early and put _hooks_ in before a change is neccesitated. We want to leave ourselves open for changes and those can be really hard to implement if the developer anticipated for too much. So what we really want to do is get the each pass out quickly, so that it can then be torn apart where neccessary. Uncle Bob likened this to _Taking the First Bullet_. In the Marine Corps we used to go on patrols in an attempt to draw fire. I know that probably sounds crazy, and it is but to get in front of our problems we have to know what they are.  When combatants shoot at you, they give you information, and a chance to counter. Complexity and rigidity are the enemy and if you can identify where they are coming from than you can prevent future attacks by _training_ toward the desired future behaviors.

So just like your DNA abstracted away your form from the abilities the form could gain, our code should be flexible too, incorporating changes as they occur. A change in requirements is met by the minimum neccesary extension of behaviors and this leaves software adaptable, flexible, but not fragile.
Here's the steps listed in PPP that help to create these kind of amourphously modeled programs:
- once a change occurs, we implement an abstraction that protects us against future changes of that kind
- we can simulate changes using
  - write test first TDD
  - we dev using short cycles
  - we dev features before infrastructure, frequently showing off the features to stakeholders
  - we dev the most important features first
  - we release software early and often


Further reading
Here's a visual walkthrough and explanation of the [*OCP*](https://www.youtube.com/watch?v=JLURCz3dDtY)

Further Reading:
See what Uncle Bob had to say about [*The Open Closed Principle*](https://8thlight.com/blog/uncle-bob/2014/05/12/TheOpenClosedPrinciple.html) in this article he wrote for [*8th Light's Blog*](https://8thlight.com/blog/).




<sub style="color:gray">
Test:
<ul style="color:lightgray">
<li>Does the intro have an attn getter? Yes
<li>Does the intro describe and define the topic covered at a high lvl? Yes
<li>Does this have some major points each mapped to a single paragraph?
<li>Does the body have one coding example?
<li>Does the conclusion summarize the information presented?
<li>Does the conclusion include at least one further reading link?
</ul>
</sub>


