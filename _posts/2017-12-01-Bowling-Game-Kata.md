Bowling-Game-Kata

# One Pinhead Short of a Pair

About two weeks ago, while at 8th Light's Libertyville office that I've fortunately been allowed to work out of a few days a week while I'm living in the suburbs; Eric Meyer, one of 8th Light's crafters, asked me while I was working on my Elixir Game of Life ([G.O.L.]), if I would like to pair with him on some swift code.  I said, "Umm... yeah, but I really have to finish my [G.O.L.] so can we do it in a bit?"  He was cordial, and said sure, but that bit never came, and we didn't get to pair, and I lost out on that days opportunity to see and absorb a crafter's code forming in practice.  Despite being pleased with my code, I didn't quite get finished with my first pass through of [G.O.L.] by my IPM the following day and the code I had was basically ripped to shreds in the IPM because it showed clear signs of premature optimization, and unneccesary modeling.  So I got nothing out of trading time that was offered by a crafter for my own lone key mashing.  I didn't deny the offer to be rude, but I did do it out of self-preservation and that is not how I'll raise the bar in professional software development. Software Craftsman value, not only individuals and interactions, but also a community of professionals.  My mentor talked to me about it later, and explained that it's better I pair with a crafter than get a story done.  Like many of my weeks in 8th Light's apprenticeship, my errs are followed by inexplicit but storied lessons that give me the chance to discover my flaws and delegitimize my hindsight biases.

# Battle Buddies

The real discovering of what standard of excellence I failed to meet in this instance wasn't fully internalized by my mentor telling me that I should pair with crafters when they ask, but instead came from the reading assignment that preceded that weeks exploits, chapter 6, _A Programming Episode_, the bowling game kata chapter, of _Agile Software Development: Principles, Patterns, and Practices_ by Robert C. Martin. The first two lines of the dialogue in the chapter, which is an account of Bob Koss (RSK) and Uncle Bob (RCM) in a hotel room in late 2000, go like this:
<blockquote>
RCM: Will you help write a little application that calculates bowling scores?

RSK: (Reflects to himself, "**The XP practice of pair programming says that I can't say, "no," when asked to help**.  I suppose that's especially true when it is your boss who is asking.") Sure, Bob, I'd be glad to help.
</blockquote>

So thinking back to my interaction with Eric, that pretty much immediately made me feel like a bad buddy. As an Infantry Marine and future crafter, I want to self enforce a partnering policy more reflective of my past experiences on highly effective teams. I want to be a battle buddy, not a bad buddy.

# Bowling Bobbies

So, okay, lesson learned, favor owed, f*** up reflected on, let's move on and see what we can learn from these two Bob buddies battling the barriers on the way to programming bowling **_together_**.

Here's the condensed version of the steps they take before starting to type in generalized terms:

1. Introduce the high level usefulness of the application (app behaviors == user stories).
2. Pick a use case (user story) to start with.
3. Determine Inputs and Outputs.
4. Pick pair roles.
5. Align functional strategy with customer expectations on form of IO.
6. Generate a sample of realistic test data.
7. Brainstorm a quick Model of the problem domain.
8. Find the end of the dependency chain, start there.
9. Red, Green, Refactor.

Throughout the chapter the keyboard slides between the Bobs.  There are some positives in the process.
- Constant Communication at _'eye level'_
<sub>What I mean by _eye level_ is that despite the one Bob being boss, they work as equal team members. They argue socratically, as peers, valuing what the other says, that's a huge part of making a good team and good teams make good code because everyone's bullshit detector and filter gets a chance to run. Since code doesn't care about your title (not talking about permissions here), and neither do your application users, let's just assume we're all imperfect human beings sitting afront the keys.</sub>
- Constant pulling of focus back to the application users
<sub>Build software for the people your building it for. Duh.</sub>

There were also some points for improvement. Sometimes I'm shocked by how perfectly tailored my mentor assigned work is, it is a little spooky that each of the following is a direct reflection of the ineffeciancies in the process I took when building my first pass on [G.O.L.], and as such, really relevant reading material.

-Premature Optimization
-Modeling through Models, instead of through Behaviors
-Programming by coincidence

# Model Citizen

After the chapter I think I'll adjust those original steps a little to make them a useful set of steps to follow at the beginning of Pairing sessions:

1. Introduce the usefulness of the application (app behaviors == user stories).
2. Pick a use case (user story) to start with.
3. Determine Inputs and Outputs.
4. Pick pair roles.
5. Align expectations of form and IO.
6. Retrieve realistic data samples.
7. <strikethrough>Brainstorm a quick Model of the problem domain.</strikethrough>Consider the highest-level testable of behavior of the application.
8. <strikethrough>Find the end of the dependency chain, start there.</strikethrough> Red, Green, Refactor.

Much better, and a nice even set of 8. :)










