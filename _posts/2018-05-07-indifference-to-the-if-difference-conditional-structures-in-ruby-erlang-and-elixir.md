---
layout: post
title: 'Indifference to the ''If'' Difference: Conditional Structures in Ruby, Erlang,
  and Elixir'
---

# Developing Polyglotism

I first learned to program through [Code Platoon](www.codeplatoon.org), a nonprofit coding bootcamp for U.S. military veterans located in Chicago. Like most web development boot camps, Code Platoon’s curriculum is mainly focused on imperative Ruby and the popular frameworks built atop it, [Sinatra](http://sinatrarb.com/) and [Rails](http://rubyonrails.org/).

I came out of Code Platoon an object-oriented programmer in an object-oriented world. I thought the nature of reality itself was beginning to unfold and reveal its cosmic configuration to me. In arrogant bliss, I falsely felt close to a complete conceptual understanding of how information is abstracted, packaged, and encapsulated. “It’s all just objects… All of it!” I revelled, like many a naive programmer before me. Then came my apprenticeship at 8th Light where the first language I was tasked with programming in was Erlang.

<blockquote>
"... [A]s an uneven mirror distorts the rays of objects according to its own figure and section, so the mind, when it receives impressions of objects through the sense, cannot be trusted to report them truly, but in forming its notions mixes up its own nature with the nature of things.”
</blockquote>
<sub>
  -
<em>
[Francis Bacon](https://en.wikipedia.org/wiki/Francis_Bacon),
[The Great Instauration](https://books.google.com/books?id=jNM7AQAAMAAJ&pg=PA250#v=snippet&q=that%20as%20an%20uneven%20mirror%20distorts&f=false)
</em>
</sub>

One of the ideals that peaked my interest in 8th Light is the principle that crafters ought to be polyglots.  I expected to learn a variety of languages during my apprenticeship but I didn’t expect the first one would be Erlang, a language considered by many to be especially difficult to learn.

Normally one does not need to understand a language to read its documentation, but the documentation for Erlang appears to be written for programmers who already understand Erlang. It also features a hodgepodge of unique abstractions, features, and frameworks that one might think need to be understood in order to write Erlang programs. The truth however, is that you don’t need to know about generic finite state machines, process mailboxes, or the Open Telecom Platform (OTP) to write a simple program in Erlang. In fact, I’d argue that for newcomers to the language, attempts to understand and use these tools will result in  more _rabbit-hole tumbling_ than learning.

When the time comes to learn Erlang in depth there are good resources available.  Most notably the work of Frederic Trottier-Hebert, who deserves a Nobel Peace Prize for all the brain bombs he’s diffused by writing [Learn You Some Erlang](http://learnyousomeerlang.com/content).

Still, I think a quick Ruby-etta stone might be helpful if you're looking to transmute some of your knowledge from one of these languages to the other. For me, comparing and contrasting languages I know with ones I'm learning has been helpful in growing my ability to quickly learn new languages and see the abstractions beyond their syntax.

# Solving the Same Problem

In the following code examples you'll see the same problem solved multiple ways in each language. If you read through each you should begin to build your own links between Ruby, Erlang, and Elixir and start to understand the translation from  Ruby-isms to Erl-ingo or Elixir-isian.

# Woof! Woof! Woof!

```
GIVEN a guard dog named Mose<br>
WHEN an intruder ([human, cat, dog]) enters his sector<br>
THEN Mose applies the appropriate bark intensity ([loud, louder, loudest]) based on intruder type<br>
```

Let's write a simple algorithm in all three languages to encode a solution to Mose’ task. First, we'll use pseudocode as an aid to find abstractions. Then we'll translate that pseudocode into tests describing the desired behavior. Finally, we'll write the code to make the test pass, doing so in as many ways as the core language constructs for conditional logic allow.

<hr>

# Pseudcode
```
1. We need to represent our actors: Mose and the Intruders...

  #=> Mose is the one taking action so he'll probably be a class or module.

  #=> The Intruders are the conditions we're switching on, and we'll only be
      getting one intruder at a time so we shouldn't need to hold a list per
      se. Maybe each intruder could be a `:symbol` (`:atom` in Elixir and
      Erlang).

2. Mose needs to have something that can look at intruders as they come in

  #=> Probably a function which performs different actions based on the
      condition of the intruder. This sounds like a conditional or branching
      logic, so let's think about our conditions.
      #=> If the intruder is a human, then Mose barks loud.
      #=> If the intruder is a cat, then Mose barks louder.
      #=> If the intruder is a dog, then Mose barks loudest.
```

<hr>

# Tests
## In Ruby w/ RSpec
```ruby
require 'rspec'
require_relative 'mose'

RSpec.describe "Bark in response to intruder..." do
    mose = Mose.new
    it "When intruder is Human, Mose barks loud." do
      expect("Woof!").to eq(mose.bark_at_intruder(:human))
    end
    it "When intruder is Dog, Mose barks louder." do
      expect("WoofWoof!").to eq(mose.bark_at_intruder(:dog))
    end
    it "When intruder is Cat, Mose barks loudest." do
      expect("WoofWoofWoof!").to eq(mose.bark_at_intruder(:cat))
    end
end
```
## In Erlang w/ EUnit
```erlang
-module (mose_test).
-import (mose, [bark_at_intruder/1]).
-include_lib("eunit/include/eunit.hrl").

bark_at_human_intruder_test() ->
[{ "When intruder is Human, Mose barks loud.",
   ?assertEqual(io:fwrite("Woof!"),mose:bark_at_intruder(human))
}].

bark_at_dog_intruder_test() ->
[{ "When intruder is Dog, Mose barks louder.",
   ?assertEqual(io:fwrite("WoofWoof!"),mose:bark_at_intruder(dog))
}].

bark_at_cat_intruder_test() ->
[{ "When intruder is Cat, Mose barks louder.",
   ?assertEqual(io:fwrite("WoofWoofWoof!"),mose:bark_at_intruder(cat))
}].
```
## In Elixir w/ ExUnit
```elixir
defmodule MoseTest do
  use ExUnit.Case
    test "When intruder is Human, Mose barks loud." do
      assert Mose.bark_at_intruder(:human) == IO.puts "Woof!"
    end
    test "When intruder is Dog, Mose barks louder." do
      assert Mose.bark_at_intruder(:dog) == IO.puts "WoofWoof!"
    end
    test "When intruder is Cat, Mose barks loudest." do
      assert Mose.bark_at_intruder(:cat) == IO.puts "WoofWoofWoof!"
    end
end
```

<hr>

## Ruby
# If, elsif, and else
```ruby
class Mose
  def bark_at_intruder(intruder)
   if intruder == :human
    "Woof!"
   elsif intruder == :dog
    "WoofWoof!"
   else
    "WoofWoofWoof!"
   end
  end
end
```

# If as a modifier
```ruby
class Mose
  def bark_at_intruder(intruder)
    return "Woof!" if intruder == :human
    return "WoofWoof!" if intruder == :dog
    "WoofWoofWoof!" if intruder == :cat
  end
end
```

# Case
```ruby
class Mose
  def bark_at_intruder(intruder)
    case intruder
    when :human; "Woof!"
    when :dog; "WoofWoof!"
    when :cat; "WoofWoofWoof!"
    end
  end
end
```
# Unless
```ruby

class Mose
  def bark_at_intruder(intruder)
    unless intruder == :human || intruder == :dog
      return "WoofWoofWoof!"
    end
    unless intruder == :human
      return "WoofWoof!"
    else
      return "Woof!"
    end
  end
end
```
# Ternary
```ruby

class Mose
 def bark_at_intruder(intruder)
    return intruder == :human ? "Woof!" : bark_at_animal(intruder)
  end
  def bark_at_animal(animal)
    return animal == :dog ? "WoofWoof!" : "WoofWoofWoof!"
  end
end
```


<hr>

## Elixir
# If ... else .. Do/End
```elixir
defmodule Mose do
  def bark_at_intruder(intruder) do
    if intruder == :human do
      IO.puts "Woof!"
    else
      if intruder == :dog do
        IO.puts "WoofWoof!"
      else
        IO.puts "WoofWoofWoof!"
      end
    end
  end
end
```
# Unless
```elixir
defmodule Mose do
  def bark_at_intruder(intruder) do
    unless intruder == :human do
      if intruder == :dog do
        IO.puts "WoofWoof!"
      else
        IO.puts "WoofWoofWoof!"
      end
    else
      IO.puts "Woof!"
    end
  end
end
```
# Case
```elixir
defmodule Mose do
  def bark_at_intruder(intruder) do
    case intruder do
      :human -> IO.puts "Woof!"
      :dog -> IO.puts "WoofWoof!"
      :cat -> IO.puts "WoofWoofWoof!"
    end
  end
end
```
# Cond
```elixir
defmodule Mose do
  def bark_at_intruder(intruder) do
    cond do
      intruder == :human -> IO.puts "Woof!"
      intruder == :dog -> IO.puts "WoofWoof!"
      intruder == :cat -> IO.puts "WoofWoofWoof!"
    end
  end
end
```
# Guards
```elixir
defmodule Mose do
  def bark_at_intruder(intruder) when intruder == :human, do: IO.puts "Woof!"
  def bark_at_intruder(intruder) when intruder == :dog, do: IO.puts "WoofWoof!"
  def bark_at_intruder(intruder) when intruder == :cat, do: IO.puts "WoofWoofWoof!"
end
```

<hr>

## Erlang
# If (Guard Patterns)
[What's the diff with Erlang's _if_?](http://learnyousomeerlang.com/syntax-in-functions)
```erlang
-module(mose).
-export([bark_at_intruder/1]).

bark_at_intruder(Intruder) ->
  if
      Intruder == human ->
          io:fwrite("Woof!");
      Intruder == dog ->
          io:fwrite("WoofWoof!");
      true ->
          io:fwrite("WoofWoofWoof!")
  end.
```
## Case ... of
```erlang
-module(mose).
-export([bark_at_intruder/1]).

bark_at_intruder(Intruder) ->
  case Intruder of
      human ->
          io:fwrite("Woof!");
      dog ->
          io:fwrite("WoofWoof!");
      _ ->
          io:fwrite("WoofWoof!")
  end.
```
## Guards
```erlang
-module(mose).
-export([bark_at_intruder/1]).

bark_at_intruder(Intruder) when Intruder == human ->
  io:fwrite("Woof!");
bark_at_intruder(Intruder) when Intruder == dog ->
  io:fwrite("WoofWoof!");
bark_at_intruder(Intruder) when Intruder == cat ->
  io:fwrite("WoofWoofWoof!").

```

<hr>

## Some additional methods the languages possess for control flow

## Pattern Matching
I think of pattern matching as a means to explicitly communicate how we’d like our program to behave in the various states we expect it to encounter when executing. We do this through explicit declaration of:
 - The messages we expect will be passed to a function.
 - The expression/s to be evaluated when that message is encountered.

In my opinion it is one of the best tools a language can provide developers.

# In Elixir:
```elixir
defmodule Mose do
  def bark_at_intruder(:human), do: IO.puts "Woof!"
  def bark_at_intruder(:dog), do: IO.puts "WoofWoof!"
  def bark_at_intruder(_), do: IO.puts "WoofWoofWoof!"
end

```
# In Erlang:
```erlang
-module(mose).
-export([bark_at_intruder/1]).

bark_at_intruder(human) -> io:fwrite("Woof!");
bark_at_intruder(dog) -> io:fwrite("WoofWoof!");
bark_at_intruder(_) -> io:fwrite("WoofWoofWoof!").
```
## Ternary Operator
# In Ruby:
```ruby
class Mose
 def bark_at_intruder(intruder)
    return intruder == :human ? "Woof!" : bark_at_animal(intruder)
  end
  def bark_at_animal(animal)
    return animal == :dog ? "WoofWoof!" : "WoofWoofWoof!"
  end
end
```

# Conclusion

As I’ve become familiar with more languages and programming concepts, I’ve begun to notice more and more, I’m having revelations of commonality before difference, whole before part, and procedure before paradigm.

<blockquote>
“... Novices [use] syntax-based organization … experts [use] more abstract hierarchical organization based on principles of program function.”
</blockquote>
<sub>
  -
<em>
[Beth Adelson, Problem solving and the development of abstract categories in programming languages. Harvard University,  1981](https://link.springer.com/content/pdf/10.3758/BF03197568.pdf)
</em>
</sub>

An algorithm is a sequence of behavior and not inherently dependent on a programming language.  As crafters we’re expected to develop a cognition and acumen around abstraction. Through exposure to a variety languages we’re confronted with the reality that languages are imperfect abstractions that are only relevant as long as they contain protocols that the computer, and the programmer agree on. When I first began to program, I tended to think about algorithms in terms of the programming language I was using to implement it. It was difficult to see the forest for the trees. I had been introduced to TDD and pseudocoding a list of steps in human language before trying to solve a given problem, but I didn’t fully understand or appreciate how much better programming is when you give yourself the chance to mentally decouple a program’s desired behavior from the details of its implementation.

Putting units of behavior into human readable language and writing test descriptors and code based upon them, means your test will not only verify that your program works as expected, but can also function as documentation and context for your program. More importantly for our purposes, if we understand a problem abstractly, then through the powers of inference and deduction: New languages might be demystified when side-by-side with examples from languages we know, and if we're lucky overtime, we'll be made indifferent, to any _if_ difference.
<blockquote>
We continually master a variety of technologies and techniques. We do not let unfamiliarity dissuade us from using the best tools.
</blockquote>
<sub>
  -
<em>
[from the 8th Light Principles of Well-Crafted Software](https://8thlight.com/blog/kevin-liddle/2013/02/18/we-are-principled-1.html)
</em>
</sub>


<sub>
  NOTE: This post represents one way to compare and contrast Ruby, Erlang, and Elixir. It is by no means a guide to understanding the fundamental differences between the languages, it focuses on small similarities between small bits of working code. [Cyclomatic Complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity) is out of the scope of this writing but developers should be aware of the merits in the idea that in most cases we should reduce the amount of linearly independent paths through our programs, which often involves removing condtiionals.
</sub>
