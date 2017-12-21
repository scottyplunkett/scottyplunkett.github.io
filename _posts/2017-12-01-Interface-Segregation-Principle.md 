---
layout: post
title:  Interface Segregation Principle
date:   2017-12-01 14:48:09 -0500
categories: technical-development SOLID
---


Some code shall we?

```ruby
class You
  def talk_to_best_friend(friend,their_significant_other)
    message_to_bestie = gets.strip
    friend.send message_to_bestie
    their_signifant_other.receive message_to_bestie
  end
end

```

This is what coupling looks like and this is what we want to avoid. It's like when friends that are dating can only be communicated with as a unit even though you might only want to talk to one of them.

Wouldn't the follow make more sense and avoid your friend's loose lipped lover from hearing private messages?

```ruby
class You
  def talk_to_best_friend(friend)
    message_to_bestie = gets.strip
    friend.send message_to_bestie
  end

  def talk_to_best_friend_and_their_partner(friend,their_significant_other)
    message_to_bestie = gets.strip
    friend.send message_to_bestie
    their_signifant_other.receive message_to_bestie
  end
end

```

**I**t **S**eems **P**robable!

Let's talk about the ISP. Not the _Internet Service Provider_ you're using... Actually that could work for an analogy, so I take it back; let's talk about _Internet Service Providers_ (ISPs). From _Wikipedia:
<h7>An Internet service provider (ISP) is an organization that provides services accessing and using the Internet.</h7>_

Comcast, TimeWarner, Cox, etc. All are examples of ISPs.  How do they provide you with access? They give you a router and a modem that allows you to connect to their network, and ultimately do your stuff on the internet.  Think about the problems that might come up, in managing millions of customers, were they to just have one single interface to their network that all their customers had to share. It would be a mess, fortunately for the telecom companies and for us to some degree there are protocols for being able to identify specific places and hardware on a network, so when we hook up a modem and then a router it allows an ISP to only deal with problems or changes in one specific place.  In other words my neighbor can call and complain that their internet is down and request a technician to come out and help them, that request doesn't affect my service. It would be very annoying to depend on my neighbors service, the seperation between the network interfaces, in the form of physical (MAC) and logical (IP) addresses gives me the chance to go on doing my business on the internet without being forced to wait on a service that I don't need. This is similar to how we'd like to Design our applications. Many client specific interfaces are better for all the distal parts of your application instead of having one general purpose interface through which all rely.

Now let's talk about the ISP. Not the _Internet Service Provider_ you're using... Now I'm referring to the OO Design Principle, the **Interface Segregation Principle**:

<blockquote>Clients should not be forced to depend on methods that they do not use. </blockquote>

