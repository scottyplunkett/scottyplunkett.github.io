---
layout: post
title: The Singleton Pattern
---

The Singleton Pattern is used when you want to eliminate instantiating more than one object for a class.

A singleton class might often be at the root of an application.  When we want to inforce the singularity of a class, we can create a public class with a public method that must be called for instantiation to occur.  We ensure that there are no public constructors, this way the instance can only be conjoured through the public method, and this interaction would probably take place at the initiliazation of the application.  Here is a brief example of a Singleton class in Java.

```java
public class Singleton
{

  private static Singleton theInstance = null;
  private Singleton() {}

  public static Singleton Instance()
  {
    if (theInstance == null)
      theInstance = new Singleton();
    return theInstance;
  }
}
```

In some ways I think of the Singleton pattern as a high level flow control statement.  Where we use if-then-else statements we control and direct the flow through our code blocks, and the singleton performs a similar action for our application (or a high-level part of our application) interactions as a whole.
