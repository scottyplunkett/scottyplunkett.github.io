---
layout: post
title: Stranger Types
category: technical-development types
---

<img src="https://www.dailydot.com/wp-content/uploads/22a/31/ee6028f42d1afd5863040b46ec6aabb1.jpg" alt="lost in the woooooods"/>

  This post is intended to give a summation of the 4 most common computer science idioms used to express type systems.  While there are various combinations of static vs. dynamic, and strong vs. weak type systems, an understanding of these two basic dichotomies will enable a general understanding of how type systems affect our code and how we encode it.  I decided to write this after I corrected my mentor on Erlang's (dynamic) type system, although he correctly described the type system, he used the wrong nomenclature... And even though I corrected him on the nomenclature, I didn't really understand what was in the name.  

<img src="https://images3.content-hci.com/int-cont/img/hca/gifs/michael-scott-i-understand-nothing.gif" alt="I understand nothing"/>

  I thought it might be a good idea to familiarize myself with these stranger types.  So sit back, shave your head, and let's turn the geekery up to integer 11!

<img src="https://memegenerator.net/img/instances/250x250/70640680/dont-be-a-10-be-an-eleven.jpg" alt="turn it up to 11!"/>

# What's a Type
  A type is *drum roll*...
  A <b>DATA TYPE</b>!!

  I know this may seem obvious to the average programmer but inference aside, I've found the terms <em>type</em> and <em>data type</em> are used different colloquially.  

  The fact is that <em>typing</em> gives a richer meaning to a sequence of bits.  To put this in perspective, try to imagine if your processor or compiler couldn't tell the difference between a string and a list of numbers...  It'd be like you were programming in Erlang or something!

<img src="http://slides.sigma-star.com/slideshows/no-string.jpg" alt="please don't string me along"/>
  

# Check Your Type, Before You Wreck Your Type: Static vs. Dynamic

The first thing we need to talk about is type checking.  

```Type checking``` describes the process of verifying and enforcing contraints on types.

  The following two adverbials tell us <b><em>when</em></b> and <b><em>how</em></b> the type checking process occurs.  

A ```Statically``` typed system verifies the type safety of a program through analysis of the program's source code text.


A ```Dynamically``` typed system verifies the type safety of a program at runtime.

<blockquote>
Notice how neither of those definitions mention compilers.  I was at first confused by the notion that Erlang is dynamically typed, because it alerts you of errors when BEAM compiles your code.  The important difference is that you can still try to execute your Erlang code, and it won't crash if their is an error until runtime.
</blockquote>

This means that you often have to declare your types in static type languages, otherwise you won't be able to execute your code.  

In a dynamic type language, you often won't need to declare your types, and as a consequence, errors won't be caught until ```runtime```, when the code is actually executed.

So let's say you are coding and you attempt to add an integer to a string, static or dynamic, let's say you declared the type of each in the proper way, what happens then?  To answer that, we'll need to understand the difference between strongly and weakly typed languages.


# Are You Strong Enough to be My Type?: Strong vs. Weak


In the previous section I asked what happens when you try to add an integer to a string.  The answer depends on whether the language is strongly or weakly typed.

The difference lies in how the runtime environment reacts to types that don't match up.  

<blockquote> It should be doubly noted that there is no agreed upon definition of these terms.  </blockquote>

A ```strongly``` typed language enforces types.

A ```weakly``` typed language will attempt a type conversion or will still try to execute code that requires operations on different data types.  


These are loose definitions and should not be taken as law.  The diversity in programming languages lends to the fact that different mixes of these systems are actually encountered when you are running and gunning (compiling and executing).

What matters more than the definitions themselves is the understanding of the processes that deliniate where they differ, or at least that's how I feel about these stranger types, but again, I'm unfamiliar. :)









