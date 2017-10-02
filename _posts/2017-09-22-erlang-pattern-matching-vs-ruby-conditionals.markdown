---
layout: post
title:  "Erlang Pattern Matching vs. Ruby Conditionals"
date:   2017-09-22 14:48:09 -0500
categories: technical-development erlang-vs-ruby
---

Let's begin making sense of how these two compare by looking at Ruby If/Elsif/Else Conditionals vs. Erlang Pattern Matching.

Using If in Ruby:

	Let's say you're a Drill Instructor with a group of recruits that are standing in formation awaiting inspection.  When you step in front of a recruit, you inspect their uniform and offer them encouragement depending on the condition of their uniform.

	In ruby you might create a Drill Instructor class object and give it a method that uses an if, elsif, else conditional statement:

		class DrillInstructor

			def give_recruit_encouragement(uniform_rating)
				if uniform_rating == 1
					"Good to go recruit."
				elsif uniform_rating == 2
					"Nasty recruit."
				else 
					"Heinous recruit! Go get on my quarterdeck!"
				end
			end

		end

	Let's try to parse the code above into it's definitive pieces so that we can better understand what makes it function. Perhaps we can avoid getting lost in translation later, when we have to think in Erlang, by applying a functional understanding of our Ruby code.

	Here are the pieces:
	1. class: DrillInstructor 
		when instantiated creates an Object that holds the behavior we gave it through our method declaration
		ex. drill_instructor = DrillInstructor.new
	2. method: give_recruit_encouragement
		we declared our method with the def keyword and gave it a name so that we can call this functionality by name on our object
		ex. drill_instructor.give_recruit_encouragement
	3. parameter: uniform_rating
		we abstractly describe the values that we expect to be passed into our method through parameters rather than describing the values themselves, when called, this abstraction is replaced by an actual value
		ex. drill_instructor.give_recruit_encouragement(1)
	4. conditional statement: if- if uniform_rating equals 1
		here we start our conditional, and begin to encode expecations (1)
	5. expression: "Good to go recruit." 
		this where we give our code to something to evaluate, or what should happen (printing a string of encouragement) if the expected value (1) is encountered
		(although it has no explicit return statement because we don't always need one in Ruby
		what we're really saying is return the string "Good to go recruit.")
	6.  conditional statement: elsif- if the uniform_rating didn't equal 1, and instead equals 2
		if our uniform_rating made it passed the first statement, here's the second most likely expected value 
	7. expression: "Nasty recruit."
		when our method is given the expected value of (2) evaluate the expression: return the string "Nasty recruit"
	8. conditional statement: else
		here we are saying it's possible we'll have something that doesn't meet our expectations, and there's a need to do something other than what we did when a 1 or a 2 is passed into our method
		this third possible value could be something like our other expectations (3), or it could be something totally different ("foobar")
	9. expression: "Heinous recruit! Go get on my quarterdeck!"
		in any case we don't get a 1 or 2 value as our uniform_rating parameter evaluate the expression: return the string "Heinous recruit! Go get on my quarterdeck!"


	That was exhaustive. Thinking functionally we'll feel that way at first.  

	You may have heard some version of the axiom: "In Ruby, everything is technically an object." This is one of those simplifications that I encountered early on when learning to code, that was totally helpful in understanding how to think Object Oriented; unfortunately, it's also a lie and in the truth lies a useful concept for those going from Ruby to Erlang: 

	http://rubylearning.com/blog/2010/09/27/almost-everything-is-an-object-and-everything-is-almost-an-object/

	Not everything in Ruby is an object, but everything in Ruby evaluates to an object.

	This is similar to the way that in functional languages, everything returns something. 

	Now, Erlang does have an 'If' but it doesn't work like the If conditionals of object oriented languages, so let's pretend the Erlang 'If' doesn't exist for a moment.

	I found that translating something like the Ruby code above into Erlang, is aided by an understanding of pattern matching. 

Using Pattern Matching in Erlang:

	I think of pattern matching as a means to communicate how we'd like our program to behave in the various states we expect it to encounter when executing. 

	We do this through explicit declaration of: 
		a. The different arguments we expect will be passed to a function.
		b. The expression/s to be evaluated if an expected argument is encountered.

	In Erlang we might use pattern matching inside a drill_instructor module to write the above like so:

		-module(drill_instructor).
		-export([give_recruit_encouragement/1]).

		give_recruit_encouragement(1) -> io:format("Good to go recruit.~n");
		give_recruit_encouragement(2) -> io:format("Nasty recruit.~n");
		give_recruit_encouragement(_) -> io:format("Heinous recruit! Go get on my quarterdeck!~n").

	
	Let's use the parts from our Ruby code to see how we might arrive at the above code.

	1. 	The Ruby class: Drill Instructor,
		becomes the Erlang module: drill_instructor
			In Ruby our class stored our behaviors
			In Erlang a module stores functions 
				NOTE: A module must have the same name as the file that holds its definition 
				-module(drill_instructor). => must be at the top of a file named drill_instructor.erl
	2. 	The Ruby method: give_recruit_encouragement, 
		becomes the Erlang function: give_recruit_encouragement
	3. 	The Ruby parameter: uniform_rating, 
		becomes the 1 in the Erlang export declaration
	4. 	The Ruby conditional statement: if uniform_rating == 1, 
		becomes the Erlang function head: give_recruit_encouragement(1) ->
	5. 	The Ruby expression: "Good to go recruit.",
		becomes the Erlang expression: io:format("Good to go recruit.~n");
	6. The Ruby conditional statement:  elsif uniform_rating == 2,
		becomes the Erlang function head: give_recruit_encouragement(2) ->
	7. The Ruby expression: "Nasty recruit.",
		becomes the Erlang expression: io:format("Nasty recruit.~n");
	8. The Ruby conditional statement: else,
		becomes the Erlang function head: give_recruit_encouragement(_);
	9. The Ruby expression: "Heinous recruit! Go get on my quarterdeck!",
		becomes the Erlang expression: io:format("Heinous recruit! Go get on my quarterdeck!~n").