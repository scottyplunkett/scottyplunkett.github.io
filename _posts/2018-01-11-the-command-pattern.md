---
layout: post
title: The Command Pattern
date:  2018-01-11 14:48:09 -0500
category: patterns
---

The chapter in [PPP](https://www.amazon.com/Software-Development-Principles-Patterns-Practices/dp/0135974445) that discusses the command design pattern opens with a cartoon of a general putting his finger in the eye of what seems to be a junior enlisted troop.  So that got me pretty confident I would understand the command pattern pretty easily seeing as I've been junior enlisted and I've had officers actually put there fingers in my eye... err, a [knifehand](http://www.businessinsider.com/6-reasons-to-fear-the-knife-hand-2015-8) probably, and it was rare that an officer did knifehanded me, more likely it was another enlisted Marine, like a squad leader.

The Command design pattern intends to separate and decouple both temporally (timing of calls to validation or execution) and physically (entirely different classes) an object of invocation from the object that receives the message of invocation.

The following ruby code demonstrates such a set-up, using Marines as an example...

```ruby
class Marine
  attr_reader :action

  def initialize(action)
    @action = action
  end

  def name
    self.class
  end
end

class SquadLeader < Marine
  attr_reader :route, :mission

  def initialize(route, mission)
    super 'Marines, Condition 1!'
    @route = route
    @mission = mission
  end

  def execute
    file = File.open route, 'w'
    file.write "#{name}: #{mission}\n"
    file.close
  end
end

class Rifleman < Marine
  attr_reader :route, :mission

  def initialize(route, mission)
    super 'Pew, Pew, Pew!!!'
    @route = route
    @mission = mission
  end

  def execute
    file = File.open route, 'a'
    file.write "#{name}: #{mission}\n"
    file.close
  end
end

class PlatoonSergeant < Marine
  attr_reader :route, :target

  def initialize(route, target)
    super "Alpha1Actual, we are moving the platoon from #{prettify route} to #{prettify target}..."
    @route = route
    @target = target
  end

  def execute
    FileUtils.mv route, target
    file = File.open target, 'a'
    file.write "#{name}: We moved from #{prettify route} to #{prettify target}..."
    file.close
  end

  def prettify(routename)
    (routename.chomp File.extname(routename)).capitalize
  end
end

# the following class keeps track of the various Marines commands...

class CompositeCommand < Marine
  attr_accessor :commands

  def initialize
    @commands = []
  end

  def add_command(*args)
    args.each { |arg| commands << arg }
  end

  def execute
    commands.each { |command| command.execute }
  end
end
```

Try putting this code into a file named platoon.rb (or your own example) and running through the steps you'll find in the blog this code was based on: (this design pattern blog by [Doug Yun](https://dockyard.com/blog/2013/11/05/design-patterns-command-pattern).
