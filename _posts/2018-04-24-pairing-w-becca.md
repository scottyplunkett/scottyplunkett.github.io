---
layout: post
title: Pairing w/ Becca
---

# Pairing w/ Becca
<hr>

Originally I was slated to pair and subsequently be reviewed by Emmanuel but he wasn't going to be around on the day of my review board, May 11th. Fortunately Becca, stepped in to save the day and helped me quickly get our two days of pairing scheduled.

Becca told me that she didn't really know Ruby or feel comfortable with it and that it was new to her, because she has been working in JavaScript projects for awhile. When she first told me this, what I thought she meant was, 'I've never written Ruby code.' So I got excited because I figured it meant I would have a chance to really provide value on this edge of the pairing tour, volunteering: "I'm pretty [**_confident and comfortable_**] with Ruby."

It turns out that Becca and I have different definitions of _comfortable_. Most of our pairing dealt with integration of an internal gem that Becca had written based off a previous gem the client used internally. Authoring a gem for production, from my perspective me is like the rubyist version of earning a black belt. It means you understand not only how to effectively write applications in ruby, but also understand how to distribute those programs and effectively modularize their behavior so that others can use them. I was impressed! I've not written a gem and I'm a person who's self-reportedly _confident and comfortable_ with Ruby.

As crafters we ought to know that we can write workable programs in any language, but just writing a program does not indicate one truly **knows** the language. And this is, I think why Becca said she '... [D]idn't really know Ruby'. Downplaying effectively made me more impressed by Becca's work and removed the possibility of me being suspicious of her ability when we ran into issues with the gem integration. Downplaying is perhaps not the right word and I'm not sure Becca would call it that. She might instead call it _setting realistic expectations_, I'm not sure... Regardless, I have gotten better at setting expectations since coming to 8th Light but still have plenty of room to grow.

As I stated we had issues w/ the integration. It was due to our confusion with the `require` method in Ruby and how it's used with gems.  The way the `require` method works is that it pulls in a specific file's code. We had installed the gem Becca wrote and we had required the gem in the files where it was to be used. Why wasn't it working? The answer is simple, _a gem name does not necessarily correspond to the ruby files it has within._ We needed to specifically `require 'gem_main_dir_name/the_specific_file'` in the application files that needed the behavior of `the_specific_file`.

At one point the client developer who sat behind Becca and I discussed this with us. He brought up _POODR_ by Sandi Metz, and I felt dumb by interjecting his point about (and listing of) execution order dependencies 'require, include, etc.' with 'delegates'. It reminded me of a point Byron brought up in a Zagaku about expectations at new clients, where he said, '... [b]e careful to not state things about programming that you aren't sure of...' Mentioning delegation in a conversation about class/file loading and order of execution made me look like I wasn't being truly present, or wasn't very smart, and wasn't a good look. I'd like to be better about letting others explain things and being able to actively listen without seemingly trying to prove I already know what they're talking... No one likes a _know it all_, and I know Becca has also read POODR and managed to listen intently while he shared what he remembered from it.

The 2nd day of pairing we did standup with a different team than Becca and Pat (who was also working at this client) normally do the standup with. Than later on we went to lunch with the 8th Light team for this client w/ the team's director Gustin. Gustin was the director for my week of pairing w/ Josh as well and he has a noticeably calming energy and seems like a great person to work with. In the middle of the day we met with the lead dev who was kind of delegating different work to the 8th Lighters involved in the engagment and he basically told Becca and Pat that thanks to their quick quality work that they no longer needed them working on the project they had been, and told them they were going to be moved onto a different project. It seems they get moved around a lot at this client, which I'm not sure makes it more or less difficult for 8th Light to position itself to grow the relationship. I also was able to sit in on a large (40ish people) client _team talk_ which seemed to be a mixture of a retro, lesson, and a brainstorming session.

I was happy Becca joined my review board not just because I needed another crafter to pair with and she stepped up, but also because before Becca, Hana was the only member of my review board with a gender different from my own. I don't want to downplay the value I gained from pairing with the review board members that have gender and race in common with me, but I've found that there are additional benefits to working with & learning from people that have perspectives derived from different intersections than my own. My interactions with Becca provided further validation that working with people who have different backgrounds than I, is often: More fun, more interesting, and often offers more oppotunities for learning.

More opporunities for learning, means more chances to grow as a person and a professional. Take my previous example: I hyped my ruby ability and she did almost the opposite. I think it's clear that as a person, showing vulnerability and as a professional, self-managing one's hubris; are not traits that are typically attributed to men, but they are _desirable_ traits for any professional.

I'm much more concerned with being a good teammate than being good at displaying the gender norms society [tells] me I should display. I think most people have that same desire, and working with people that we're different from is one way we're able to be make ourselves more open & extensible team members. So I'm glad that Becca and the other members of the DCI team are actively putting forth effort to increase diversity at 8th Light. I felt I should emphasize my thoughts around diversity in this blog, especially because I know it's something that's important to Becca as well.






