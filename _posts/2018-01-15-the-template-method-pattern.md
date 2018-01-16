---
layout: post
title: The Template Method Pattern
date:  2018-01-15 14:48:09 -0500
category: patterns
---

So in the early 90s and days of OO when objects first began to have the capability of inheritance, programmers were swooned by the this concept's promise in code reuse.  This led to overuse of inheritance, because well, [programmers are lazy](http://threevirtues.com/).

In [PPP](https://www.amazon.com/Software-Development-Principles-Patterns-Practices/dp/0135974445) the template method pattern is defined in contrast with the strategy pattern.  This is because where one uses delegation (the strategy pattern) to solve the problem of seperating a classes generic algorithm from the detailed context, the other uses inheritance.  Both still solve the same problem already mentioned: seperating a classes generic algorithm from the detailed context, the other uses inheritance. This is core to not violating the D of the SOLID design principles, the _Dependency Inversion Principle_:
<blockquote>
  <li>
    High level modules should not depend on low-level modules. Both should depend on abstractions.
  </li>
  <li>
    Abstractions should not depend on details.  Details should depend on abstractions.
  </li>
</blockquote>

So, the template method pattern uses inheritance, but how?

A really helpful example in swift can be found in my mentor [Nicole Carpenter's blog here](http://nicolecarpenter.github.io/2016/05/02/template-method-pattern.html).

One common example is by seperating a game loop:
```ruby
class Game
  def loop
    # do something while not done
    ...
  end
end
```
From a detailed implementation that inherits from said game loop:
```ruby
class TicTacToe < Game
  def initialize(pieces, board)
    @pieces = pieces
    @board = board
  end
end
```
As you can see we've seperated the generic algorithm, a game's looping behavior, from the detailed context of TicTacToe. That is essentially the template method, and I like it because it kind of quickly gets the high level abstract functionality out of the way for you when creating an application.  That can be much harder if trying to be done after the details have been encoded.

