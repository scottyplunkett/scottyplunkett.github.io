---
layout: post
title:  Dependency-Inversion-Principle
date:   2017-12-01 14:48:09 -0500
categories: technical-development SOLID
---

What happens when you rip off a lizards tail? Does it's DNA change? Does it become a different lizard? Does it die?

In case you actually aren't sure what happens, the tail pretty much just comes right off and the lizard scurries away. If you see that same lizard a few days later, a tiny stub will be forming where the tail once protruded.  A few more days and you'll notice that stub has grown a bit more.  Some months later, a new fully functional tail is right back where you had removed one. In all the lizard catching experiences I had during the times I lived in California as a child I never saw a lizard die because it's tail came off. That's a pretty great defense mechanism for change wouldn't you say?

<blockquote>
  <li>
    High level modules should not depend on low-level modules. Both should depend on abstractions.
  </li>
  <li>
    Abstractions should not depend on details.  Details should depend on abstractions.
  </li>
</blockquote>

I wonder about the code that constructs the tail and what kind of interfaces exists within the lizard. Modeling our programs, even object oriented programs after the real world can cause problems. However, I find it useful to consider how we might design software systems _around_ problems solved in the natural world. Lizards require much more to operate than we'll need for an example, so let's think about the lizard chase in code but keep it in scope of the tail/scurry issue.

```ruby
class Lizard
  attr_accessor :tail

  def initialize
    @tail = LizardTail.new
  end

  def scurry_away
    @tail.counter_balance
    p "Scurries fast!"
  end

  def drop_tail
    @tail.detach
  end

  def regenerate_tail(time)
    @tail.grow(time)
  end
end

class LizardTail
  attr_accessor :length
  def initialize
    @length = 2
  end

  def grow(time)
    if @length == nil
      @length = time
    else
      @length += time
    end
  end

  def detach
    @length = nil
  end

  def counter_balance
    if @length > 0
      p "Tail Swings #{@length/2} to the right."
      p "Tail Swings #{@length/2} to the left."
    else
      p "Tail can't help with balance, right now."
    end
  end
end
```

Let's make a lizard.
```ruby
lizzy = Lizard.new
=> #<Lizard:0x007fcdc2874cf8 @tail=#<LizardTail:0x007fcdc2874a28 @length=2>>
```
Great! That works.
Let's see if `lizzy` will scurry.
```ruby
lizzy.scurry_away
=> "Tail Swings 1 to the right."
   "Tail Swings 1 to the left."
   "Scurries fast!"
```
Awesome, she's fast!
...but is she fast enough to get away from a predator who grabs her tail?
```ruby
lizzy.drop_tail
=> nil
lizzy.scurry_away
=> NoMethodError: undefined method `>' for nil:NilClass
   from (irb):91:in `counter_balance'
   from (irb):59:in `scurry_away'
   from (irb):103
```

Hmm...

If our lizard wants to scurry away it can't if the tail were to be dropped. In other words, in the case of a single change, our lizard breaks. This is because we created a dependence for the lizard (High Level) on the lizard tail (Low Level).

In real life a lizard doesn't depend on it's tail to scurry.  In fact it doesn't really depend on it's tail at all, it's more of a defense mechanism. Recently, it was discovered that the tail that grows back, is not even a true lizard tail!

<iframe width="560" height="315" src="https://www.youtube.com/embed/GEehfX6qUwM" frameborder="0" gesture="media" allow="encrypted-media" allowfullscreen></iframe>

In real life what happens is the lizard details: all it's parts, how they function and interact, [depend on the abstraction that is the lizard DNA](https://www.huffingtonpost.com/2014/08/21/lizards-regrow-tails-humans_n_5694899.html). The DNA is sort of like a policy layer. Then a level down we have the lizard mechanisms, like regeneration. One more level down we have the actual implementation, a tail. This way the lizard can operate and be lizardly without having it's little life depend on such a lossy tail. It's unfortunate we don't have the same regeneration mechanism, but we do have the same drive to survive in the face of physical losses i.e. amputation, blindness, etc. This is thanks to a SOLID design that we share with all known living creatures. Perhaps these principles of OO design are not an invention so much as a discovery. I wonder what other discoveries await our kind as we grow technologically... I don't know, but I know the only way to find out is to keep coding.


