---
layout: post
title: Refactoring
categories: Agile
---

# The 3 Sacred Functions of a Module

I felt I got a lot more out of Eric M's refactoring Zagaku than I did from Martin's chapter in [PPP](https://www.amazon.com/Software-Development-Principles-Patterns-Practices/dp/0135974445) but I did like the focus toward simplicity. I also like that he describes refactoring as an effort towards purpose. Essentially there are 3 purposes of a module that Martin identifies.

# Execute
The primary purpose of a module or class is to execute a set of functionality for a program.  If it cannot execute it has no purpose or at the very least is defeated at its core purpose. Refactoring should make the execution of a module quicker, cleaner, and more focused. Some refactoring effort should be made directly toward optimization but on the whole I find that optimization is secondary to understandabaility and simplicity. Often a good design makes optimization easier because, functions are changeable without breaking the module in its entiriety.  Good design, as we know, comes organically from TDD and agile practices.  

# Change
Another purpose of a module is to be maleable.  It should be easy to change and ease of implementation of changes is another focus of refactoring. In refactoring we do this through simplifying individual functions and responsibilities within a module or class itself. This ensures that identify the crux of a change is a quick process.  

# Communicate
The final purpose of a module is to communicate it's purpose. How readable a module or class is for other developers is directly linked to the time it will take for the implementation of a module in the greater program it serves.  In refactoring we should look for areas of confusion in our modules and adjust our names in tandem.  