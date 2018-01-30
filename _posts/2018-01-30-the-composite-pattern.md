---
layout: post
title: The Composite Pattern
---

# Let's Make a Stick Figure

<img src="http://images.all-free-download.com/images/graphiclarge/stick_figure_clip_art_23608.jpg" alt="stick figure">

In the image above you see a fully formed stick figure.  The fully formed intended picture is really made up of individual pieces but what we [see is the _elephant_](https://en.wikipedia.org/wiki/Seeing_the_elephant), so to speak.

# Let's Rip a Stick Figure to Pieces

Let's imagine we had to make the stick figure in code.  We could write code to draw a stick figure but that seems a little obnoxious and painfully complex; imagine how long the function that draws a whole stick figure might need to be. Plus if we were to write such a function, it would seem to be quite hard to extend in a meaningful way and we'd probably end up appending more and more lines onto the stick figure drawing code block. The issue is that a stick figure is not a trivial shape... But it is _made_ of trivial shapes...

Let's count the shapes...

Stick Figure:
- 5 Straight Lines: The Body, Left Arm, Right Arm, Left Leg, and Right Leg. <img src="http://freevector.co/wp-content/uploads/2013/10/59549-minus-horizontal-straight-line.png" alt="circle">
- 3 Circles: The head, Left Eye, and Right Eye. <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a0/Circle_-_black_simple.svg/1000px-Circle_-_black_simple.svg.png" alt="circle">
- 1 Curved Line: The mouth <img src="http://glyphwiki.org/glyph/left-half-circle@1.svg" alt="curved line">

```ruby
class Shape
  def draw
    # some kind of vector drawing
    ...
  end
end

class Circle < Shape
  ...
end

class StraightLine < Shape
  ...
end

class CurvedLine < Shape
  ...
end

```

Okay so we've demonstrated that a stick figure can be broken into it's components and simplified.  But there does still need to be a stick figure at the end right?

# The Composite

Just like a beautiful painting needs a sturdy canvas we need some kind of object that can hold and compose the pieces of our shapes... This object is kind of a shape itself, and that is how we'll view it when implementing the composite pattern.

```ruby
class StickFigure < Shape
 delegate :draw, to: :shape

 def initialize(shapes)
  @shapes = shapes
 end

 def add(shape)
  @shapes << shape
 end
end

```

What this pattern enables us with is the ability to treat what could have been a one-to-many relationship (StickFigure to Shapes), as a one-to-one relationship (StickFigure to Shape).

:)-><


