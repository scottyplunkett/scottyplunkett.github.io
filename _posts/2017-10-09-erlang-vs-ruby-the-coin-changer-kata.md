---
layout: post
title: 'Erlang vs. Ruby: The Coin Changer Kata'
---

I spent a lot of time over the past 2 weeks doing the Coin Changer Kata.  In the following post I'll discuss my approach to this Kata in both Ruby and Erlang.

As craftsmen we use Katas to showcase and practice the ability to attain a _flow_ while coding a program.  The term Kata is borrowed from Martial Arts-

_From WikiPedia:_
Kata (型 or 形 literally: "form"), a Japanese word, are detailed choreographed patterns of movements practiced either solo or in pairs.

___

Ruby:
<iframe width="560" height="315" src="https://www.youtube.com/embed/4TEiV8sqikA?rel=0" frameborder="0" allowfullscreen></iframe>

The above video is from the apprenticeship of Chris Peak, a designer at 8th Light.  The way I do the Coin Changer in Ruby uses a similar approach.

Tackling the Coin Changer problem through a Kata, gives us the opportunity to see how complexity is driven through simple advances in our test, and follows simple changes in code to meet the increasing complexity of a problem.  This is illustrated best in the move between our code making change for 1-4 cents and our code making change for 5-6 cents.  

This upswing in complexity is due to the fact that prior to needing change for 5 or 6 cents, we really only require pennies, and our code thus takes the form of a pattern match.  
<blockquote>
In pattern matching all we need to do is simply declare our expectations in the form of conditional statements, and then explicitly state expressions or values to return upon those conditional statements evaluating to true.  Pattern matching will help us when we write the Kata in Erlang, so take note of that definition.     
</blockquote>
Once we ask, through our test, for our code to return a 5 or a nickel and a penny once a value greater than 5 is passed to our method, we have to change our strategy.  We get into imperative coding because we can not explicitly state what we need our code to do.  

It's interesting to note, that the increasing complexity of the problem is not necessarily a sign that we need to increase the complexity of our code, in fact our code at the end is less lines and seemingly more simple than it is when we our only dealing in pennies.

Here's my Ruby Code at the end of the coin changer Kata:

```ruby
require 'rspec'

Penny = 1
Nickel = 5
Dime = 10
Quarter = 25

def make_change(amount)
	coins = [Quarter, Dime, Nickel, Penny]
	change = []
	coins.each do |coin|
		while amount >= coin 
			change << coin
			amount -= coin
		end
	end
	change
end

describe 'make change' do 
	it 'makes change for 1 cent' do 
		expect(make_change(1)).to eq([1])
	end
	it 'makes change for 2 cent' do 
		expect(make_change(2)).to eq([1,1])
	end
	it 'makes change for 3 cent' do 
		expect(make_change(3)).to eq([1,1,1])
	end
	it 'makes change for 4 cent' do 
		expect(make_change(4)).to eq([1,1,1,1])
	end
	it 'makes change for 5 cent' do 
		expect(make_change(5)).to eq([5])
	end
	it 'makes change for 6 cent' do 
		expect(make_change(6)).to eq([5,1])
	end
	it 'makes change for 8 cent' do 
		expect(make_change(8)).to eq([5,1,1,1])
	end
	it 'makes change for 10 cent' do 
		expect(make_change(10)).to eq([10])
	end
	it 'makes change for 14 cent' do 
		expect(make_change(14)).to eq([10,1,1,1,1])
	end
	it 'makes change for 15 cent' do 
		expect(make_change(15)).to eq([10,5])
	end
	it 'makes change for 18 cent' do 
		expect(make_change(18)).to eq([10,5,1,1,1])
	end
	it 'makes change for 20 cent' do 
		expect(make_change(20)).to eq([10,10])
	end
	it 'makes change for 23 cent' do 
		expect(make_change(23)).to eq([10,10,1,1,1])
	end
	it 'makes change for 25 cent' do 
		expect(make_change(25)).to eq([25])
	end
	it 'makes change for 88 cent' do 
		expect(make_change(88)).to eq([25,25,25,10,1,1,1])
	end
end
```

Erlang:
<iframe width="560" height="315" src="https://www.youtube.com/embed/dNllpVZt_Ks?rel=0" frameborder="0" allowfullscreen></iframe>

The above video is my screencast of the Coin Changer Kata in Erlang in real time.  

I found it more challenging to make simple changes in code to meet the increasing complexity of the problem when tackling the Kata in Erlang.  This is in part due to the fact that in Erlang we have to use tail recursion.   

I was familiar with the concept of recursion prior to the Kata... In recursion we call a method on itself.  Tail recursion is a specific recursive process in which we call a function on the head element in a list and then call the function again on the rest of the elements in a list repeatedly until a base case is meant.

Here is my Erlang code at the end of the coin changer Kata:

```erlang
-module(coin_changer).
-include_lib("eunit/include/eunit.hrl").
-export([make_change/1,make_change/3]).

make_change(Amount) -> make_change(Amount,[25,10,5,1],[]).
make_change(0, _, Change) -> Change;
make_change(Amount, [HighValue | Coins], Change) when Amount >= HighValue -> make_change(Amount-HighValue, [HighValue] ++ Coins, Change ++ [HighValue]);
make_change(Amount, [_ | Coins], Change) -> make_change(Amount, Coins, Change).

coin_changer_test_() ->
	[
	?_assert(make_change(1)  =:= [1]), 
	?_assert(make_change(2)  =:= [1,1]),
	?_assert(make_change(3)  =:= [1,1,1]),
	?_assert(make_change(4)  =:= [1,1,1,1]),
	?_assert(make_change(5)  =:= [5]),
	?_assert(make_change(6)  =:= [5,1]),
	?_assert(make_change(7)  =:= [5,1,1]),
	?_assert(make_change(8)  =:= [5,1,1,1]),
	?_assert(make_change(9)  =:= [5,1,1,1,1]),
	?_assert(make_change(10) =:= [10]),
	?_assert(make_change(13) =:= [10,1,1,1]),
	?_assert(make_change(15) =:= [10,5]),
	?_assert(make_change(17) =:= [10,5,1,1]),
	?_assert(make_change(18) =:= [10,5,1,1,1]),
	?_assert(make_change(23) =:= [10,10,1,1,1]),
	?_assert(make_change(25) =:= [25]),
	?_assert(make_change(75) =:= [25,25,25]),
	?_assert(make_change(88) =:= [25,25,25,10,1,1,1]),
	?_assert(make_change(94) =:= [25,25,25,10,5,1,1,1,1]),
	?_assert(make_change(99) =:= [25,25,25,10,10,1,1,1,1])
	].
```   



