---
layout: post
title:  "Functional Programming Introduced"
date:   2017-10-05 10:48:09 -0500
categories: technical-development functional-programming
---

This is the first in a series of post I'm writing to explore functional programming as I'm learning about it.  Coming from imperative languages and object oriented programming, my hope is to wrap up some of the concepts in brief posts that other developers making the same mental leap between programming styles will find useful as quick reference material.

I'd like to credit the creators of [LambdaCast](https://soundcloud.com/lambda-cast), whose podcasts are the basis for these series of post.

---

## What is Functional Programming?

Functional programming is a programming _paradigm_, in other words, _a method or pattern for writing a computer program_, in which computation is driven by the evaluation of mathematical functions, rather than changes in state or mutable data.   

So, rather than creating and thinking about our applications in terms of _objects_, or _who_ exist inside our program; _their defining features, and descript methods these objects hold within themselves_... 

We instead create and think about our applications in terms of _functions_, or _how_ data is passed around our application; _what actions must be performed, and what should these actions return_.

<blockquote>
An overly simplified comparison: <br>
With OOP (Object Oriented Programming) you think about what goes into a program. <br>
With FP (Functional Programming) you think about what comes out of a program.
</blockquote> 

#### What isn't Functional Programming?

It's important to note that functional programming does not require a declarative, functionally driven language.  Functional programming is a way of thinking, that although may lend itself well to declarative languages, is not a categorical explanation of those languages.  One might find that programming in an imperative language can benefit from a functional approach.

---
# Anything you can do, I can do better: First-Class & Higher Order Functions

<iframe width="560" height="315" src="https://www.youtube.com/embed/WO23WBji_Z0?rel=0" frameborder="0" allowfullscreen></iframe>

# Functions as First-Class Citizens
In computer science, there are entities described as first-class citizens.  These entities support all the operations generally available to other entities.  These operations typically include being passed as an argument, returned from a function, modified, and assigned to a variable.  In functional languages, and in functional programming, functions are treated as first-class citizens.  So this means we can use functions in the place of other first class data (like an Integer) and our program is cool with it.

An example using different First-Class Citizens in Javascript:


# Higher Order Functions
Higher Order Functions refer specifically to functions which take other functions as arguments.

An example in Javascript using the First-class Function and Higher Order Function concepts: 

```javascript
const twice = (f, v) => f(f(v));
const add3 = v => v + 3;

twice(add3, 7); // 13  
```

---

# Nothing Else Matters: Pure Functions

<iframe width="560" height="315" src="https://www.youtube.com/embed/tAGnKpE4NCI?rel=0" frameborder="0" allowfullscreen></iframe>

Pure Functions are functions that take some input/s, operate **ONLY** on those inputs, and return something.  

A pure function doesn't need anything other than it's arguments, **nothing else matters**, and whatever it returns is something predicatable and did not modify the arguments themselves. 

Example of a pure function in Javascript:
```javascript
function add(X, Y){
	return X + Y
}
```

The add function doesn't care for what the X does in the rest of the program, and it doesn't care for what the Y knows about the rest of the program, but it knows what it is supposed to do: return the two arguments added.  Anytime we pass the same inputs to this pure add function, we know it will return the same output.  This is predicatable behavior.  We also know that X didn't change, and Y didn't change; what was returned was something different from X and Y, and both can go on living their life unmutated.

<blockquote>
All pure functions are total functions. Total functions <b>ALWAYS</b> return something.  The inverse of a Total function is a Partial function, a Partial function might not return something. 
</blockquote>

Pure functions are a principal concept a Functional Programming.  In Functional Programming, as much of our program as possible must be expressed using pure functions.  


	
________