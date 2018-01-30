---
layout: post
title: The Observer Pattern
---

If like me you found the Observer Pattern chapter in [PPP](https://www.amazon.com/Software-Development-Principles-Patterns-Practices/dp/0135974445) to be a bit heavy on process, to the point the pattern gets lost in the code examples and becomes hard to understand in the end, than you're in luck. I'm going to simplify it.

The observer pattern works in a few parts:

# Something has got to change...
*First* by having something something that can change.

```elixir
defmodule ThingThatChanges do
  def change
    # some type of change
    Subject.notify
  end
end
```

# The Subject at Hand...
*Second* by having a `Subject` object that can do two things `register` and `notify`.

```elixir
defmodule Subject do
  use Agent

  def register(new_observer) do
    Agent.update(Observers,  fn state -> {state, state ++ new_observer}
  end

  def notify do
    Agent.get(Observers, for observer <- Observers, do: observer.update)
  end
```

# Thanks for letting me know...
*Third* by having observer object/s that implement an `update` function.

```elixir
defmodule Observer do
  def update do
    # some kind of notification of the change
  end
end

```

The whole process is really just a matter of maintaining a list of subscribers that can be appended to, and having the module that maintains said list, be accessible or commanded from the thing that changes, then enumerating over each of the subscribers calling their notification function whenever the changing object tells the list maintainer (subject) it's changed.

And for some more code and examples of design patterns I recommend taking a look at the Erlang examples from [_Design Patterns for Simulations in Erlang/OTP_](http://freebsd.csie.nctu.edu.tw/pub/distfiles/erlang-doc/r12b1/master_thesis_patterns.pdf):

Here's a spoiler that I think gives a good preview and description of how the Observer Pattern works.


```
Lets say that the people behind ”The Search for Extraterrestrial Intelligence”
needs to notify people around the world when they eventually discover life in
outer space. To accomplish this they can implement a seti-subject like the
following, which allows computers around the world to attach themselves and
then receive updates whenever contact to outer space is established.
```


I'm beginning to really appreciate functional Erlang and it's nicer looking sibling Elixir. They're kind of beautiful to observe...
