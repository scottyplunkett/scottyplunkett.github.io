---
layout: post
title: 'Pairing w/ Hana'
---

Day 1:
Today I didn't get to write any code.
This client's office was really cool, although most of the team was out for a meeting with the customer in New York. (We were subcontracting on the project through the client, who ultimately answered to the customer)
Without being too explicit about the project we were working on; it involed a bluetooth medical device for reading information from a patient that was connected to a mobile application, by which patients were set-up with coaches and able to track and monitor their health data and progress toward goals assigned by the coaches.
Initially patients and coaches were set-up under the same DB table on the backend which was managed through a Phoenix app (Elixir) reachable through an Absinthe Graph-QL API.
We began to separate the users table which included both coaches and patients into two separate tables.
The goal was to keep them separate since the patients will be treated very differently from a coach from a business logic perspective due to the fact that patient health & identifying data must be protected.

Day 2:
Today I did get to drive while Hana navigated for most of the day.
It was a little awkward getting used to Hana's keybindings and VIM in general.
She was super patient though.
I would have liked to have worked in Elixir on the backend (just because I love writing elixir code) but our goal for the day was to catch up the mobile app, written in Swift, with the work we had done the previous day on the elixir part of the system.
Mostly this involved, changing any place where once there was a User implementation or interface, to a patient implementation or interface.

Day 3:
Today what we did was work on getting the React app up to speed with the backend. The React portion of the system was on the web and intended to act as a dashboard for the coaches.
I had a good bit of trouble figuring out where we were in the system.
There was a lot of context switching.


Day 4:
Today we spent most of the day trying to get push notifications working.
Push notifications must be tested on a device. Somehow the emulator in XCode can't be used to test push notifications. Some of the day was spent setting up two devices with which we were handed to test the push notifications. There was also sparse documentation on actually getting the notifications to display. We didn't have much trouble implementing the boiler plate code from MS AppCenter (used in tandem with MS Azure for to manage the application) and that got us to the point that we knew we were receiving the notification but it wouldn't actually display on the device.
We also had the team IPM today, which I was glad to witness. Kevin K came in to run the IPM and it was interesting to see the relationship between 8th Light and a client who's interested in being better. During the IPM everyone was able to talk over what they'd gotten done during the last (3 week) cycle, and voiced any concerns they had. At one point testing was brought up, and one of the client developers seemed genuinely interested in developing their TDD skills, even to the point they asked Kevin K about it after the IPM.

Day 5:
I woke up & got to work late today which fortunately Hana was very understanding about. I still was able to get in for some work on the push notifications in the morning before our 8th Light Company Stand-up, but wasn't able to get them working before lunch. Hana through me a shout-out for bringing enthusiasm to the team and project during the week which was really nice of her to do and made me feel appreciated.
I held a mini-retro in the afternoon during Waza time w/ Hana. Which I'm glad I did for a couple of reasons:
1. She was pleasantly surprised when I asked her if I could get feedback from her during waza which probably helped me save face in regard to that morning.
2. She was able to give me good feedback: I should have stopped her when we were pairing and I didn't know where in the code we were, or what the specific piece of code we were working on was meant to do. This was a good piece of feedback because throughout the week I felt like I was making a lot of suggestions but they were rarely the correct suggestion to be making. Had I slowed us down and made sure I fully understood the lay of the land, I might have been a bigger help in making us effective during the week.
3. I was able to give her my perception of the week and explain to her that like all 8th Lighters I've paired w/ she brought a sense of stoicism and focus to work with her. I also told her that this week was the funnest pairing week I've had thus far because we got to work on a cool product, w/ interesting and diverse technologies, at a client who was easy to work with.

