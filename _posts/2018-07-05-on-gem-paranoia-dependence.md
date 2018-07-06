---
layout: post
title: "On Gem Paranoia & Dependence"
---


Most of us start off learning to do such mundane things in programming.
“Put this string in reverse…. Arrange this list... ‘Hello, World!’ that... and ‘Kon'nichiwa sekai!’ this…”

It’s no wonder that gems are so tempting to early stage developers and lazy programmers that want to do cool things. Many a programmer are attracted to software development for the very fact that it enables them to build things... Build cool useable things, and less so because they love to sort lists alphanumerically.

I loved gems when I first started developing. The first time I threw [Geocoder](http://www.rubygeocoder.com/) in a rails app during Code Platoon, I was blown away. Which is why you won’t find me completely bashing the use of gems. Let’s consider the positives…

# Gems are a pleasure...

They are incredibly easy to set up, and in more ways than one. They take very little time, usually seconds, to get up and running in an application. They’re also highly accessible in that they have a clearly defined standard packaging api in the [GemSpec](https://guides.rubygems.org/specification-reference/). Most other languages are frankly just awful at simplifying their packaging process. [RubyGems](https://www.RubyGems.org), the Ruby community’s package manager & hosting service is free, simple, friendly, and ubiquitous. You can literally publish a Ruby library to RubyGems, in the matter of a minute or so after signing up… Wouldn’t it be nice if building, packaging, and publishing applications in other languages were so easy? Or are you some kind of masochist who likes dealing w/ Maven? If not, you must agree that Ruby is such a pal when it comes to idiot proofing the development process.

The other great thing about gems is that they can lift a lot of burden off of the developer and in limited cases take care of some of the more important details that we often need in an application but don’t have the expertise or time to encode. Oh, you don’t understand how to handle authentication? Nobody does! You don’t need to be a cryptography expert to roll out user authentication in a Ruby app because smart people have already done the important parts for us with gems like Devise. Or maybe your client’s internal users would like an admin interface to see their Rails application apart from all the pomp and circumstance you throw at the external users. With Active Admin, you can have it going in no time…

# Gems can be a pain...

Which brings us to the negatives and the motivation for this post. Recently while working on our [Managed Services Team](https://8thlight.com/services/managed-services/) I’ve had the opportunity to delve into the more awnry parts of the gem experience. One of our client’s uses a Rails application for asset management & reporting of public utility equipment. A great deal of that management is visualized through an ActiveAdmin powered interface. It enables their administrators to gain a more high level view of the state of things and interact with resources in ways that they might not want the average user doing.

A number of the stories I’ve pulled involved reconfiguring ActiveAdmin. One day it was making the admin panel more mobile friendly. Another day it was turning off batch actions for specific resources. Each time it’s been somewhat of a challenge to bend the gem to my will. Gems in many cases are not malleable, and I would argue that in terms of the ability to manipulate what the user is presented with in the view, ActiveAdmin is less than ready for change.

One particular story was extra challenging. I was tasked with removing a big “Restore Asset” button from the admin panel’s show view for assets that were not archived… From the customer’s perspective it’s pretty easy to see why you wouldn’t want to see a giant button that indicated something was deleted or archived when it wasn’t. It sounds simple enough, right? How hard should it be to remove a giant button? Removing things is almost always easier than creating new things… but alas we’re in external dependency territory. A frightening place indeed. Which is why I’d recommend taking a pragmatic and defensive posture in regard to gems in production code bases.

# Oh, CRUD...

ActiveAdmin calls CRUD (Create, Read, Update, Destroy) features actions and there are different kinds depending on the scope or view of the assets you are interacting with. In common with Rails, you have show, index, edit, etc. views of particular resources. ActiveAdmin provides you with some built in helpers for customizing and configuring the controllers that it generates for you when you install the gem; which have their own verbiage but nothing to out of sync. There are batch and collection actions which both involve interacting with multiple resources, and then there are action items and member actions which deal in individual items from the show and index views respectively. In an application with multiple layers and associations it can be somewhat tricky identifying on which ActiveAdmin controller you should place a custom action if you need to but in this particular case all I needed to do was remove a button that shouldn’t be there in the first place.

I located the views that were affected with the unwanted button but there was no code that I could plainly see to delete. I knew what I was looking for was an action item method, but in the generated controllers there was no custom restore action. It was driving me nuts because I would delete some other action item code, and watch its associated button disappear. Yet, the one piece of malarky that I needed to delete would persist, and nowhere in the code could be found its declaration.

I read the [ActiveAdmin docs](https://activeadmin.info/) again, I googled once, then googled again, and I checked Stack Overflow and there wasn’t much help... Not even the typical snarky comments… When no one has thrown shade on stack overflow at your question, you know you’re in deep water. The advice that I did find from the interwebs was worrisome. Some spoke of using custom javascript to simply hide the button. I really didn’t like the idea of moving our users from a state of misinformation to a state of disinformation by shadowing an active component that had no business being there in the first place.

# Hammertime...

There was one line that I had noticed in the controllers that I hadn’t touched because it’s call alone made me paranoid about the implications of fiddling with… I mean it literally read ‘active_admin_paranoia’. I eventually asked Sarah Sunday what it was and she confirmed it was a call to the [ActiveAdminParanoia](https://github.com/raihan2006i/active_admin_paranoia) gem which has an affect on the actions available for a particular asset. I deleted the line and like awesome Ruby magic everything was gone… Everything, including a bunch of functionality that we really didn’t want to be gone! Our admin users wanted to be able to restore assets that had been deleted, they just didn’t want to see the extemporaneous option to ‘Restore’ when it was convoluting their admin panel.

I first went through a series of unsuccessful attempts to override the gem method with a custom restore action of my own but this gem really weaved its way into the base controller and I’m not a huge fan of overriding the behaviors of a superclass if they’re problematic anyway. I fretted, but then I remembered something I had heard the week prior from [Justin Holzmann](https://github.com/7hoenix) about testing to check if some requirement or dependency is the source of an issue when you’re debugging by literally hardcoding a different file in place of the dependency.  It seems like an obvious diagnostic technique, and is good practice for the pragmatic programmer but I had never ventured into the territory of changing a structurally integral external dependency for production code. I did however learn and watch Becca Nelson wade through the issues involved in switching out a gem dependency during my pairing tour, where we went through process of modifying and upgrading an internal gem dependency for another client that she had written.

# Gems can be absorbed...

Looking over the source code for the gem the lines that were causing the unwanted button to appear were glaring and it was clear that the only option was to switch out the code and use our own version. Here’s the general steps to pulling in and testing your own version of some external gem dependency:
1. Fork the code from the author. You could just clone it down but if you accidentally push up to their master: Best case, you’ll look like a noob, and worst case, you’ll wreck something that we already know at least one application needs. The goal of this process is not to change their code but to potentially change ours.
2. Clone down the code from your fork of the repository.
3. Go inside the application’s Gemfile and remove or modify the source for the gem you are working on. There’s no need to change the source for the rest of your gems. Just add a comma and a path option to the gem that is relative to your [Ruby $LOAD_PATH](http://joshuapaling.com/blog/2015/03/22/ruby-load-path.html.)
```
Ex. gem "custom_gem", path: "/home/username/path/to/custom_gem"
```
4. Run bundle install to update gem source to your local copy.
5. Make a branch, and fix up the code you must, then run their test, and write your own if possible. Then commit, and push to your fork of the gem.
6. Make sure once you’ve pushed back up, that you make a separate commit to change the gem source back to a version controlled copy living on the web.
Ex. gem "custom_gem", source: “https://github.com/geminicrickets/custom_gem”
7. It’s also a good idea to transfer ownership of the updated gem’s repo to a version control entity that is less vulnerable to change. In our case we transferred ownership to the github account for the client rather than keeping their app dependent on some external source like my github or the RubyGems version.
Internalizing to some level of control, application dependencies that were previously out of your control is safe, might help avoid versioning issues, and makes you more able to react to changes from users and systems.
7. Be a nice person and put in a pull request in to the author. They may like the changes or they may not, but they shared code with you and you ought to feel obliged to share your derivation with them, even if it is a [tiny change](https://github.com/raihan2006i/active_admin_paranoia/pull/14/commits/069cf9c2037f8c25642bbdb07cbfb550203aeefd).

After all the pain of finding the problem; I only had to remove the one method from the gem to get the code where the client wanted it...

For reference, this was the method:

```ruby
action_item :restore, only: :show do
  link_to(I18n.t('active_admin_paranoia.restore_model',
  model: resource_class.to_s.titleize), "#{resource_path(resource)}/restore",
  method: :put, data: {
    confirm: I18n.t('active_admin_paranoia.restore_confirmation')
  })
  if authorized?(ActiveAdminParanoia::Auth::RESTORE, resource)
end
```
###### The I18n Internationalization Gem is never far off from any Gem problem in my experience

# Gem Addictions

The problem with gems is that they’re like a gateway drug to careless dependencies. Not to throw out a [slippery slope](https://yourlogicalfallacyis.com/slippery-slope) fallacy but we’ve all seen gem-heads... They get hooked on gems because they make you feel invincible. Your features become shinier, and you build them out faster. You even look cooler to your friends that don’t know how to use gems, and you wonder why anyone would ever stop using them… It’s not use but abuse that we want to avoid. Here are some reasons why you might want to eliminate gems whenever possible:

They are a dependency and come with other dependencies with dependencies. It should feel like a big scary tree of codependency and willful ignorance every time you run Bundler and watch the gems fly by like your reading Matrix code.
You don’t control their maintenance. There’s little doubt that your environment and the gem’s development environment will change at different rates. They can have unexpected effects and touch parts of your code you don’t want them to. They’re so magic you don’t even know it… They prevent a need to disillusion programming concepts and learn the cool stuff they’re doing.
They set unrealistic expectations. Let’s say one day you throw together a Rails app with file uploads, geolocation, authentication, etc. Your users were impressed... But now they’re expecting a similar delivery on other complex features...

# Gem Nuance

Like most things in programming and life, when you first learn about something or hear an opinion you should take it with a grain of salt. I thought Gems were great because they’re incredibly easy to set up and some do such interesting things for you. Not so ironically those are closely related to the same reasons I would now caution against using them. That doesn’t mean they’re all bad. Many are downright awesome. Still, I think that having a healthy dose of paranoia when it comes to gem dependence is a good way to be. I want to be clear… there’s nothing wrong with using or writing a gem. We all owe a debt of gratitude to the gem writers and maintainers out of the world, but don’t be too afraid to be one yourself. Especially if you catch yourself already in throws of a dependency bug in a production code base like I did. I’ll end with a good rule of thumb: _Be more paranoid of needing a dependency than afraid of losing functionality._


