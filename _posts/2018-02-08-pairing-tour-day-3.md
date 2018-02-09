---
layout: post
title: 'Pairing Tour: Day 3'
---

Today James and I worked through a complete refactoring the parent component of the list we worked on [the day prior](https://scottyplunkett.github.io/2018/02/07/pairing-tour-day-2.html). In contrast to the day before, I drove the majority of the time, which was nice because I felt like with mine and James' powers combined I was able to see messy code made cleaner, smaller, and clearer. We genuinely made a difference for the users, plus current and future developers of an application that will be used, thus, we made the future an easier place to be. This is a good feeling, and I will chase it.

James started our pairing by quickly explaining that the strategy we would undertake would involve commenting out the entirety of the component, it's template, and the associated test file, then rebuilding from the test out, piece by piece until all the functionality that mattered was still there but the code was better. In the end **we removed** _**150 lines from the component, and 100 lines from the component's & template's associated test file**_. Pretty mind boggling considering it also worked better and the tests more accurately expressed the behavior of the motifs they described.

Near the end of the day, I'm proud to say that I helped James solve the first problem that I saw him actually struggle to quickly dismantle. Maybe I shouldn't be too proud because it was a simple mix up of a function argument name, `userInput` vs `input`, being called improperly. Nonetheless I spotted it, and we were both relieved from the daunting red plaguing our console and test runs.

I also was able to review an incredibly long set of changes from our commits throughout the day. This task was tedious and I didn't like it much, but I struggle to refine my commits and double check all my work before PR's anyway so I was due for some eyeball hazing and knew that the once over must be done for us to be confident that we did right by the client and didn't waste someone else's time who'd have to fix the errors later were any to be detected. At some point James also removed an insane amount of trailing whitespace and `var` vs. `const` variable declarations.

I am proud of the work we did and thankful to work with someone better than me. What a fulfilling activity it is, _writing good code_.


