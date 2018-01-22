---
layout: post
title: The Null Object Pattern
---

With the _Null Object_ pattern we can eliminate the need to check for null evaluations, and simplify our code.

Oftentimes when we are executing a function on or for a resource, we first check if that resource exist, something like this example from [PPP](https://www.amazon.com/Software-Development-Principles-Patterns-Practices/dp/0135974445):

```java
Employee e = DB.getEmployee("Bob");
if (e != null && e.isTimeToPay(today))
  e.pay();
```

According to Martin, this is an ugly an error prone idiom. One method to alleviate errors, is to have the DB.getEmployee function throw an exception instead of returning `null`; but wrapping such logic in try/catch blocks is usually even uglier than the check for `null`.

This is where the use of the _Null Object_ pattern can come in handy.

Essentially what we'd do is create an Employee interface that is implemented by both a standard `EmployeeImplementation` object and also a `NullEmployee` object.  In the `EmployeeImplementation` we'd have all the variables and methods a standard employee should have for operation; and in the `NullEmployee` object we'd have all the same variables and methods, yet they'd return or do some form of _nothing_.

Using this pattern we could then implement the above code as follows:

```java
Employee e = DB.getEmployee("Bob");
if (e.isTimeToPay(today))
  e.pay();
```

This way we are more communicative about what we are trying to do without the need for control flow in the form of the `&&` operator in our if statement. The implementation of `Employee` will be of the form of the null object, where of course we do not need to worry about a payment occuring because the pay action is rendered useless inside of the `NullEmployee` object.
