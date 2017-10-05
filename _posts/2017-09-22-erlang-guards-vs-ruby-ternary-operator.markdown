---
layout: post
title:  "Erlang Guards vs. Ruby Ternary Operator"
date:   2017-09-22 14:48:09 -0500
categories: technical-development erlang-vs-ruby
---

Follow along by forking the [example code](https://github.com/scottyplunkett/erlang-vs-ruby) or copy and paste from the below.

---

#	Using A Ternary Operator in Ruby:

Let's say it's 2007 Iraq and you're an 18 year old Marine standing post, protecting 30 lives and millions of dollars worth of government property.  18 year olds, trained or not, still have adolescent minds, so it's important that if we're going to have them guarding the base, we'd better have given them a really simple procedure for dealing with threats.     

In ruby one really simple and succinct way to encapsulate the Marine on post and a simple procedure for dealing with an oncoming threat would be through creation of a GruntOnPost class object with a react_to_threat method that the infantryman can use when trying to quickly determine if they should engage potential threats or not.

```ruby
class GruntOnPost
  def react_to_threat(threat_distance)
   threat_distance <= 25 ? "Mozambique Drill." : "Stay Frosty."
  end
end
```

Pretty succinct, right?  That's because we used the ternary operator.  

Now if you're wondering what a '[Mozambique Drill](https://en.wikipedia.org/wiki/Mozambique_Drill),' is: 
It's a drill that Marines use to convince folks who want to do harm that instead they should take a nap on the floor forever.  
More importantly, in the above code it represents what happens when the threat is within a distance value of 25; 
and conversely, 'Stay Frosty,' represents what happens if a threat is not inside 25.


What's interesting about this, is the ternary operator looks at a binary condition (a condition that is always either true or false), and then **always** returns something based on said condition.  

Wait, ternary operations **always** returns something?  
How functional of you Ruby! 

It makes sense then to look at our expression as an opportunity to declaratively express a base case for when **guarding** the post is most urgent... which is almost exactly how we write & think about guards in Erlang.

____

# Using the Erlang Guard 

{% highlight erlang %}
-module(grunt_on_guard).
-export([react_to_threat/1]).
react_to_threat(Threat_Distance) when Threat_Distance <= 25 -> "Mozambique Drill!";
react_to_threat(_) -> "Stay Frosty." .
{% endhighlight %}

Just like in our Ruby code with the Ternary, using Guards in Erlang, there are two potential situations (binary condition), the guard `when Threat_Distance <= 25` evaluates to false or it evaluates to true, and we tell our Grunt to behave differently in scenarios when the guard clause evaluates to true.


In case my 10,000lb. analogy didn't hit you yet: Like Marines standing post, Guards maintain situational (state) awareness and allow the program to be evaluated only when their statement evaluates true.

Erlang Guards = Naps


