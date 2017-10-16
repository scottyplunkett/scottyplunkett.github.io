---
layout: post
title: 'Understanding the 4 Simple Rules: Expresses Intent'
category: technical-development simple-rules
---

# Overview

This week I was assigned the reading, _Understanding The 4 Rules of Simple Design_ by, Corey Haines.  The following series of post are my thoughts on each of the rules and some notes on how Corey helps us understand them.

# Expresses Intent

### Expresses

This almost seems like it should go without saying but in practice is much harder to incorporate than a lot of the other primary skills we're asked to develop as crafters.  

I think another way to say this would be, "Write code that other people can read..."  

Go take a look around stack overflow.  Do you see a lot of X's, Y's, and aliased variables that represent working code that is nearly impossible to digest?  I do.  

<img src="http://i.imgur.com/G8c4eXx.jpg" alt="explain that to me like i'm 5"/>

The above image from the office should give you a good idea of the situation we are trying to avoid.  If we stay focused on the fact that we aren't writing code for ourselves and we exhibit self-lessness in attuning our descriptors, be they method/function names, class/modules names, etc., than we give other people the chance to appreciate our code the way we do.  

### Intent

The second word of the phrase is <b>INTENT</b>.

This extends out the meaning behind this rule.  One could say, "Write code that other people can read... and speaks to the intended behavior of the code."

This means there is a difference between

```ruby 
def input
	gets.chomp
end
```
and
```ruby
def get_console_input_from_client
	gets.chomp
end
```

Perhaps the example is a little simplified but you get the point.

We want to be specific about the intended behavior of our code so that others can clearly parse through our application and see when, where, and why a piece of functionality was brought in to the code base.  It also gives us the opportunity to write more expressive test and extend our code between its parts easier.  


