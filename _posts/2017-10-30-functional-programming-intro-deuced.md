---
layout: post
title: Functional Programming Intro-deuced
categories: technical-development functional-programming
---

In this post, I'll be introducing the concept of recursion as it relates to functional programming.  Recursion is an important tool used in programming whether you are taking a functional approach or not but in the case you are using a functional language it's practically required knowledge.  

Recursion can be a confusing concept at first, because it seems so meta.  At it's highest level recursion could be described as calling a function on itself.  

```ruby
def print_list_of_numbers(num)
  if num === 0
     puts 0
  else
    puts num
    print_list_of_numbers(num - 1)
  end
end
```

In the example above we see a few parts.  The first part is the base case.

# Base Case

```ruby
if num === 0
  puts 0
```

A base case describes when in our method or function we want to break out and stop the execution.  In the example above we tell our ```print_list_of_numbers``` method to stop executing once the ```num``` is equal to 0.

# Recursive Call

```ruby
  else
    puts num
    print_list_of_numbers(num - 1)
  end
```

The recursive call happens when we encounter anything other than the base case.  In the above example we tell our method if anything other than zero is encountered as ```num``` then ```puts num``` (print num) and call yourself again with number minus 1.

So if we passed in 10, the method would be called over and over again 10 times until the method was called with 0 as the num parameter.

Another important concept to understand is tail recursion.  This is a method of recursion where you make recursive calls on all elements in a list except for the first.  You can see a good example of tail recursion in my Erlang example in the [Coin Changer Kata](../../../2017/10/09/erlang-vs-ruby-the-coin-changer-kata.html) post.   

