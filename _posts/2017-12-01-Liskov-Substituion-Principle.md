---
layout: post
title:  Liskov-Substituion Principle
date:   2017-12-01 14:48:09 -0500
categories: technical-development SOLID
---

### True or false:

#### A human is a square.
<sub>(No not you nerd, I mean all humans.)</sub>

*False.* √

#### A square is a rectangle.

*True.* √

#### A right arm is an arm.

*True.* √

A square *is a* rectangle... At least in our reality. We often make an effort in our code to mirror actuality, but in mimicking our level of perception we can easily and unwittingly cause violations of the LSP. How we hand off the abstraction of a square or a rectangle to go and do square or rectangle things is why we need to think about design. The LSP is a means for creating inheritance hierarchies that are less likely to stir up a violation of the OCP and less likely to cause confusion for the readers of our code.

The Liskov Substitution Principle (paraphrased):
<blockquote>Subtypes must be substituable for their base types.</blockquote>

So is a square a rectangle? Yes, but if you're writing a program that sets dimensions of shapes, you might want to avoid inheritance of rectangle-y functionality in a square class. The reason being that width and height could be set to different measurements in a rectangle...
```ruby
module Shape

  class Rectangle
    def set_width(width)
      @width = width
    end

    def set_height(height)
      @height = height
    end
  end

end
```

While a squares defining feature is that the width and the height are always equal.  So if you wrote...

```ruby
module Shape

  class Square < Rectangle
  end

end
```

It wouldn't seem unreasonable, but it could very well cause problems later on.  You'd probably need to end up overriding the general rectangle functionality. Validity is not intrinsic...
  <blockquote>A model viewed in Isolation cannot be meaningfully validated.</blockquote>

Let's look at another example where we properly separate related classes.  We all exist somewhere on a spectrum of right to left handed.  But we generally don't share all behaviors between both arms. I'm fairly ambidextrous but still have an arm of preference for most activities. In the case I don't, the objects we interact with are made more efficient by using the *designed-for* arm. I write left-handed, but still shift gears with my right hand. So it would make sense, I think, to code out arms like so:

```ruby
module Body

  class Arm

    def initialize(hand)
      @hand = hand
    end

    def wave
      @hand.raise
      @hand.shake_around
    end
  end

  class RightArm < Arm
    def shift_gears(from_gear,to_gear)
      @hand.shift(from_gear,to_gear)
    end
  end

  class LeftArm < Arm
    def write(anything)
      @hand.write(anything)
    end
  end

end
```


So if a set of classes all support a common responsiblity, they should inherit that repsonsibility. If a common superclass doesn't exist, create one and move the responsibilities there. Dirivatives shouldn't do less than their base classes, so if you find yourself overriding behaviors of the super class, you should consider making an entirely seperate class rather than using inheritance.


Further Reading:
See what Doug Bradbury had to say about [*The Liskov Substitution Principle in relation to the Decorator Pattern*](https://8thlight.com/blog/doug-bradbury/2016/07/18/not-a-decorator-ruby.html) in this article he wrote for [*8th Light's Blog*](https://8thlight.com/blog/).
