#--------# Digital Humanities Project #--------#
#-- Cade Garcia
#- Oct 31, 2024 

# # # # Loading Libraries # # # #
library(dplyr)
library(conflicted)
library(tidytext)
library(stringr)
library(stats)
library(wordcloud2)
library(magrittr)

# # # # Loading in MANUALLY scraped interviews # # # #
#> Most recent interview as of today. Retrieved from: https://www.interviewmagazine.com/culture/donald-glover-interviews-donald-glover
Current_interview <- c("On my farm.",
                       "He kindly stopped for me -",
                       "Yeah. I love farms. I love produce. We’ve got all these fruits, bees. Our cow is pregnant so we’ll have milk for a little while. It makes me happy.",
                       "A tangelo. I love them. These and the black mulberries might be my favorite. Which won’t be ready for a while. But they’re really good. My kids sit underneath the tree and go nuts.",
                       "It’s this one piece from Bode doing that. I gave it a French flair with the hat. They just opened a spot in L.A.",
                       "When I’m here. I like being functional. My partner, she paints a lot so you might brush up against some color. And with kids you have to wear things that look better as they get dirtier.",
                       "I guess I don’t love interviews and I asked myself, “Why don’t you like interviews?” And I think part of it is that the questions are usually the same. This way I can get questions I usually don’t get asked.",
                       "I don’t think it’s more contrived than any other interview.",
                       "Great",
                       "In what sense?",
                       "Oh. Willy Wonka. That’s the world I like. You have your factory, you make something, put it out, and then close shop to the public for a while.",
                       "I don’t think life is real unless some things are just for you. Things that should not or cannot be shared. I think the younger generation is going to have a hard time distinguishing whether something is for them or for others, and I think it could play out as a diminished sense of self. You really have to know what you would do if no else was watching.",
                       "Exactly. I mean, how do you know otherwise?",
                       "I was putting my son to bed a few nights ago and asked him what he was thinking about. He said, I’m thinking of the last long dream I’ll ever have.",
                       "I thought it was beautiful.",
                       "Oh yeah, all the time. I’ve been rapping a lot. Producing. It’s fun again. Doing features.",
                       "I did one that I don’t think is gonna come out. I think the artist, or the artist’s management, thinks the verse is too controversial.",
                       "I think that’s the game. A lot of people out here are celebrities. So their value is in people liking them. I believe my value is in my vision. So I have to make something good enough and just be human. You can get torn apart for anything, true or not.",
                       "Yuck. Can we not?",
                       "I just... ew.",
                       "That wasn’t about cancel culture. You think it’s a lot of tweeting?",
                       "No. I suppose I’m not. I think artists are supposed to interpret the world around them. And the internet isn’t the world to me.",
                       "I was kind of intentional. I was born in the ’80s, so I had a good context of how I felt without it. And I felt better without it, so I stopped. The internet moves like a drug. An accelerant. I don’t know.",
                       "Everyone I meet who’s active on the internet looks tired as fuck in real life.",
                       "I’m not. I just grew up.",
                       "I don’t think that’s true. People don’t think of life the way we used to. Because we communicate online and everyone has a personal brand. If Malcolm X was alive right now and lived the same life, people would say he “rebranded himself.” No, he just was Malcolm X. He evolved through his experience.",
                       "[Laughs] Yeah.",
                       "It’s old footage. I think he has some interesting ideas, but I disagree with him on that one.",
                       "Yeah, I guess. But then where does it end? How would I know if something’s for me if it’s all a political act?",
                       "Why are you asking me that?",
                       "I feel like you’re using Black women to question my Blackness.",
                       "[Laughs] Well, yeah? Should someone else?",
                       "Can I say something? I hate talking about race more than five minutes unless it’s with other Black people and/or we’re laughing.",
                       "In what way?",
                       "I definitely think it’s diluted in the marketplace. Because everyone can do it and it doesn’t have to be authentic. It happens every 10 to 15 years. I think we’re at the tail end of it now, though.",
                       "I think just focus on your perspective, not your “Blackness.”",
                       "In some ways I don’t. But I also don’t want them to be the light-skin kid saying, “I don’t see color.”",
                       "I guess I just want them to be good men? But they decide what they are. I’ll love them regardless.",
                       "Someone who knows themselves. Who loves themselves. My father was really gentle with us. Physically and mentally. It felt radical at the time. I really miss him.",
                       "It felt radical. Probably because he was the only man I knew like that growing up. There’s physical danger in men-love. If someone doesn’t understand what they’re feeling, it could get violent. I don’t think women usually have to deal with that, unless they’re dealing with men. But to be clear, this isn’t me saying men are bad, but the worlds are just different.",
                       "No. I didn’t have to be Earn anymore.",
                       "Yeah. But classic creative differences.",
                       "What does it mean to be a friend? I still like her. I assume she still likes me.",
                       "Maya Erskine.",
                       "Yeah. She’s dope. It’s exciting. I really love the show. I’m writing the finale now.",
                       "The word is overused. But I think, in its essence, culture is just us telling stories to each other we all remember from being god.",
                       "I think culture is just us telling each other the truth. Being present together. I bring up god because I think there’s no “we.” And that’s how I make things now. I think of the moment I wanna give someone. But I make sure that someone is me, not “us.”",
                       "I hate niche. When I was really starting to make stuff back in the early 2000s, niche was the wave. Because it was safe and you can eat off it. Back then you could eat off it a lot. But I think it didn’t challenge people to figure out if they were great or not. This ain’t a job to me. I’m in the influence game. And I think my world is better than most people’s. So I’m trying to make the most people believe in my world. That’s relevancy.",
                       "Maybe it depends on what you value. Ye always seems relevant. But at what cost?",
                       "You kind of asked me this already.",
                       "Tarantino, I guess. You make high quality shit for a large audience, you own it, and you go at your own pace. But I’d rather be influential than relevant.",
                       "You define success. Just like you define failure. So it’s really about autonomy. A lot of people you see on the internet HAVE to be on the internet.",
                       "Jesus. Choose one.",
                       "Well, wait. I’ll answer them all: You are who you are.",
                       "I think you know I’ll say what I think, you’re just milking me.",
                       "There’s good takes, there’s bad takes, but most of them are just untrustworthy takes. I liked it more when Camp just came out and it felt like everyone hated me. Because there’d be some actual good insight and it was easier to see who was dealing with their own identity problems, who really hated me, and who just didn’t like me because I didn’t dislike myself. But you can’t believe the good or bad stuff now because it’s all just the economy around you. There’s money and clout in loving and hating you. You have to sift through and try and see if someone is debating in good faith. The internet doesn’t provide a large-enough amount of that. You know what? I’ll tie in my Joe Rogan and Chappelle answer now, too. A lot of people believe both of them are doing what they do in good faith. It’s not cynical. It’s not CNN or Fox. It doesn’t feel to most people like they’re trying to sell something. People are looking for other people to interact with in good faith. Because a lot of this rage is artificial. People have emotional diabetes and don’t even know it.",
                       "Yeah.",
                       "Not at all. I like parts of that album and I learned so much. Mostly that concept doesn’t outshine content. But when they’re in equilibrium, it’s extremely potent. I don’t think I was clear on that album, and the songs weren’t catchy enough for me. Made it feel like novelty. Because I wouldn’t bop any of the songs in the car now. Maybe a couple of the hooks. But it allowed me to make Because the Internet, which I think has proven itself to be a classic.",
                       "It’s the rap OK Computer. It’s prescient in tone and subject matter and it’s extremely influential. And I know no one’s gonna give me that until I’m dead. But it’s true.",
                       "Yeah.",
                       "No! I like that show. But it does bother me when Atlanta’s compared to it.",
                       "You have to think of it like food.",
                       "No. Although I do feel like the flavor is artificial in some sense. The organic show should be about a white rapper who’s more successful than his Black peers from the jump. Because he’s more accessible. But what he actually wants is to be part of the culture, but his success keeps him from that and a lot of his Black peers and friends resent him for it but also feel like they have to fuck with him because it’s good for them. That’s the internal struggle I see. Anyway.",
                       "Yeah. It’s sadder. What are you gonna do?",
                       "No, it should be what it is. Like, people think I’m pretentious. I can be a snob. But I think in entertainment or art it’s important to know the difference between things. Like, Anthony Bourdain wasn’t pretentious, but he definitely knew the difference between a dry aged wagyu and a smash burger. Neither is better or worse than the other. They’re just different experiences. And I wouldn’t want to have either every day. But I would never confuse the two. I expect the same of my audience.",
                       "I don’t think so. You have to be honest. Especially with other Black artists. Because I think we all think we have to support each other, which we do, but sometimes supporting each other is telling someone, “This isn’t good, to me.” And you have to know why, and not be a hater, and they have to take that criticism in good faith. Then we can grow together. Then we can help each other’s recipes. You’re not helping them if they think they’ve made a “sophisticated” beef patty and it’s really a homemade tuna sandwich. Again, both can be good.",
                       "It’s a good burger you should eat fast because the ingredients are fresh. By a guy who didn’t study at a culinary school, but paid close attention to other burger spots and has the plug on good ingredients.",
                       "Right. It’s a great burger.",
                       "A really good butterflied chicken in the restaurant attached to an old hotel having a resurgence. It tastes really good and you feel guilty eating it because it’s got foie gras. But after going there for six months, you realize you always leave a little hungry.",
                       "I do, for what it is. But I do think it’s time for Zendaya to choose up and leave Sam to come to Death Row.",
                       "How To with John Wilson and Abbott Elementary.",
                       "Lemme think about it while I eat this [tangelo].",
                       "Give me a sec though.",
                       "[Laughs] You know that Kodak Black meme?",
                       "I really hope so. The barber episode of season two got the lowest score on The A.V. Club, but I was getting texts from everyone from home like, “This is the best one.” It reminded me we still gotta do both.",
                       "No. I get bored. Farming is where it’s at.",
                       "I mean farming everything. Talent, ideas, moments. You ever heard of Bauhaus?",
                       "I started reading a lot on them and Black Mountain College. It got me more interested in farming from the ground up, specifically culture. If the internet is the open wilderness, I built a greenhouse. A safe, organic place to grow talent, ideas, and product to place in the ecosystem. See if it thrives on its own.",
                       "I hope not in a dictator sort of way.",
                       "I don’t know. You remember that movie about Tonya Harding that came out?",
                       "Yeah. There’s a scene where she’s not getting scored correctly because they don’t think she represents what America is. And she asks, “Well, can’t it just be about skating this time?” I relate to that. I think language, especially in the U.S., has been destroyed. In France you gotta go through the fuckin’ court system to change a word. I don’t like when something is really about promo, but it’s being advertised as really about the art, or the culture around the art. I think that’s why awards shows have been having trouble. They don’t reflect what they actually are.",
                       "Losing my mother.",
                       "I think it’s not even that. I think it’s more I’m just in love with her right now. It brings me a lot of joy to give her joy. Our relationship now is something I’ve never known. I get to be the caretaker. She’s teaching me how to be old. I think I’m seeing her as a woman and not my mom for the first time.",
                       "No.",
                       "Are we?",
                       "Myself.",
                       "I’m serious. Sometimes you have to meditate on what you really want and what’s really happening. You know how with kids, when they experience a traumatic event, you’re supposed to go over it again and again when they bring it up to help them process it? We need to do that with ourselves. Life is incredible and traumatic. So sometimes I just sit alone and go through it: “You were born. You lived on an air force base. Your first memory was your mother giving you a peach…” You account for it all to yourself to remember you’re not Donald, you just are.",
                       "Yes. In a good way.",
                       "In a way like you’re scared for the first day of school. It’s something new we all have to do.",
                       "Just making sure Atlanta’s good. Working on a project with Dominique Fishback, Damson Idris. Chloe Bailey, too.",
                       "One hundred percent. Season three is really good, but season four is even better. Me and Hiro talk about it a lot. I’m not saying this to be pompous. I’m saying that because we deserve it.",
                       "The people. The people need to know this is high-end shit. I’m saying Atlanta is osso buco served with risotto, prepared by a chef who studied in New York City, spent five years on the road, worked at a Michelin star spot for three years, and used the money to buy a small farm. He invites you over to try out some recipes he’s been working on with his friends using the produce they grew together. Even if you don’t like it, you can’t say it’s not high quality. The quality is undeniable.",
                       "Oh! I forgot to say I’m opening an actual invite-only food spot this summer. It’s real. Meaning, not metaphorical. I love it.",
                       "Always.",
                       "My pleasure."
)

#> Older interview scrapped from YouTube. Retrieved from: https://www.youtube.com/watch?v=xhBntF0q6jY
Older_interview <- c(
  "just decided like items I was just
honoring all the time like I was just
like I was constantly checking on like
what people thought of me or like what
people were saying about me or anything
and like it was just you stop listening
to yourself",
  "and I just was like oh I'm not gonna do
that like we made a list of rules like
that I had like put on like a wood board
like a big like there it was kind of I
called it the temple the house because
it was a big buddha statue like in the
front like foyer would you come in and i
got this like engraved wooden plaque a
big plaque and it was add five rules on
it and i was like you know no
instagramming no twitter here work
starts at 10 a.m. don't invite anybody
unless like we've approved and and we
just i just wanted to set up a space
where people like really had to deal
with themselves",
  "at first it was really bad", 
  "yeah well I would go I deleted off my
phone so I'd go turn out like when
you're not doing anything you realize
like you know you're all just talking
with a friend and then your friend like
goes to get water and you're like okay
I'm gonna check my Twitter and then it's
not there it's deleted and you're like
oh man I do this every time I'm not
doing something like you do like you
check your Instagram you check it which
I'm not saying is a bad thing I'd I
think a lot of people thought this album
was like an indictment that's not like
it's I really do love the internet",
  "absolutely not I did
not to learn any", 
  "I learned what I am like and as the
things like you know you
know your faults and then you just like
you just know them you just like you
just know them and then like you can eat
it and if you care enough you change
them and if you don't then they are just
something that you just have to be aware
of",
  "I definitely like I love
the internet like I really do I've
everything my news everything about it
like everything about the world comes to
III I think my whole vision of life has
been changed by this tool that humans
are made",
  "life is high school man",
  "it's a weird thing like I mean I
don't know if it's good or bad like I
mean I definitely laugh stuff off and I get so I get like
troll jokes and you know I'm like I'm on
4chan like I'm ones though I like I get
that kind of thing",
  "but like life is high school like you got to learn to like
deal with that like you got to learn to
like okay like sometimes people don't
mean that sometimes they do but people have different acids",
  "never ends like that like life is
like high school I call it high school
just because like it's the time where
you are first figuring out like okay
socially business stuff is acceptable
like I can in socially do that but
that's what it is like there's gonna be
bullies and getting people who don't
like you just because you are who you
are",
  "there's people I
don't like because it's like you got a
mom or my brother says which is true
he's like you got to realize  not
everybody thinks like you do and not
everyone has the same code and people have different codes",
  "so I feel like my code is very
different especially growing up in this
time",
  "it's a drag but",
  "I know what kind of person I am",
  "I'm the kind of guy like like I
stopped a fight the other day like I
know a lot of people don't do that", 
  "a lot of people would see a fight and be
like yo that's not me",
  "yeah he was like this dude
punching like two strangers I didn't
know them. It's like a guy and a girl
beating up this girl and like a lot of
people were around but they didn't do
anything",
  "that's the thing I don't blame them I'm not like you guys
are bad people I'm like I don't know
what they went through in their lives
like maybe maybe their cousin got
stabbed trying to stop a fight like I
don't know their code is different from
mine my code is like I was like I have
to stop it",
  "I don't
think that's like dope or like I'm an
awesome person like it's it's stupid on
some level like it's not like you don't
know what people are up to", 
  "I think it always says that it bothers
me more than other people I know I'm
sensitive like I'm a very sensitive
being like that's just what I what it is",
  "I have to go by that that code like I am I'm honest
about as much as I can be I feel like
that's helped me a lot",
  "I'm kind of where I'm part of a group of
people who have grown up on like people
can see me that's how I've got a reason
like that's having a reason I'm even
here like I'm even famous if that people
saw me grow up",
  "it's weird meeting like 28 year olds and
30 year olds who are like 'man like yeah
like I remember when this' and then you
meet 13 year olds who are just realizing
like a derek comedy and Childish Gambino,
they all grew up with me it's kind of
this weird kind of cool thing
with like oh it's like like we're having those",
  "we affect each
other in a very different way like we
also like Michael grow up you know like
and we might do the same thing with
Theriot remember Theriot from vine",
  "kid from Atlanta who got really popular on vine
because he was like his cousin like ooh
ooh and there's like dancing and stuff
like that",
  "Theriot is like six I think or seven and
he's in the club he's getting paid right
thousand dollars to show up and I'm like
yo like we're responsible for that
like now we're responsible for Justin
Bieber we see everything like we were
responsible for Rihanna like we are just
as much that's why people want to take
selfies with me",
  "like people don't ask to
take pictures anymore they like can I
take a selfie",
  "they want
to selfie because they want to be part
of your life which is part of their life",
  "movies aren't doing as big
as they are as much as like Rihanna
concerts because people want to be a
part of it they want to be like I was
there",
  "life is now is a film like life is an
expression like my life is online and I
post pictures and so is yours like
everybody's like it's an expression it's
a movie", 
  "you can't live alone anymore really", 
  "I want to be in the picture I want to be in the
picture with this sunset like I have to
or like I see your sunset and I'm like
well I'm gonna take a better picture of
the sunset.",
  "I'm gonna put the filter on it but
I think on a sunny yeah it sucks on one
level but I don't know it's like kind of
cool that it's documenting that it's
around forever forever",
  "I live like outside ",
  "I try and keep it away from my family but that's about it
like my choices are okay like I'm already there I'm already on
the outside like me trying to get it
back it's kind of silly to me it's like
it's already there I believe like you
know the allegory of the cave
type shit like it's like oh I see it
I'm there it's all good.",
  "I just don't leave the house 
just like someplace and just get I and and just
get high by myself or read a book but
that's about it but like 
that's the thing is like I even then I can tweet
and people could know where I am",
  "you're never really alone", 
  "we're not being as responsible with the
Internet as we should be or could be",
  "I think people dont realize how like even the fact that like like Louise ek said some stuff that was really true hes like reading youre
like on there and you say like oh like
this person is like a blah blah blah you
say something like when you were a kid
you said that to someone and they hurt
their feelings or made them feel like oh
man and then you saw that effect like
everybody in class was like oh yeah that guys the blah blah blah and then like
now they dont have any friends you have
to deal with that whether or not you are
like that affects you or how it affects
you its like you still have to deal
with that you dont have to deal with
that online you can say the worst thing
to someone",
  "I know people who get online and be like I'm glad
your parents died",
  "you don't take
responsibility you don't have to you
don't have to feel that and that's what
trolling is and I get it like I'm not
against bullying like that's the world
but like at the same time like there's a
bunch of stuff ",
  "we haven't had the
internet that long",
  "I'm not
against like bullying because I feel
like the world is bullies like there's
so many people who are gonna tell you
that what you're right your ideas stupid
or like you don't belong here or you
don't deserve certain things",
  "I'm saying like
we probably wouldn't have Kanye West
Bill Gates or anyone without someone
being like you're not worth it like we
wouldn't have them and like we wouldn't
have people who were like no this is
worth it because if everybody if
everybody's ideas was worth it",
  "you need somebody to keep being
like no I'm right ",
  "you need a Joan of Arc who was like no I'm right I'm right I'm
right I'm right even if it's crazy",
  "I'm a salty dude though 
yeah I remember everybody who system like I
mean like I got beats with people like I
remember stuff I remember totally and
I'm like most of the things I do is just
like nah I'm right and I believe in
myself",
  "not vengeful I'm salty",
  "vengeful is like I'm gonna come and destroy you I
think like it's like I'm gonna win ",
  "I'm salty which I mean I think salty is
a different thing because I'm like I'm
just gonna prove you wrong I'd much
rather you sit there and be like huh I
was wrong because then I think that's
that's growing like there's lots of
times where I've done stuff in me like I
was wrong about that and I didn't even
know and then it makes you like look at
things differently on experiences in
your life like I want to make people
think like you were wrong because like a
lot of people think they know me and
they don't that's what I think that's
the thing I get most salty about is like
people think they know everything about
me they have no idea how I
grew up or the experiences I've had",
  "I remember my boy Pham came to like my
house in Atlanta and he was like I can't
believe you grew up like this",
  "well he grew up in like crenshaw in LA like he was like in
the hood he's like your house is small",
  "people like think I
was just like some sort of rich kid or
something and I can get that",
  "I understand like probably from the music
or just like the persona or just like
what I've gone through but like people
think they know me especially since they
kind of have grown up with me I've been
on the internet since I was 18",
  "yeah music is like information it should be
free it's like it should be free I
believe that and I know like a lot of
people disagree with me on that like a
lot of artists, my friends,
were like you know how am I
gonna make a living kind of but I'm like
you just have to change like people
people didn't like someone told me
something that's really true and it's
like corporations or like companies are
once they become thick enough they're
actually or actually they're actually
working to protect the problem they fix
which is true",
  "the music industry doesn't want music to be
free because that's how they've always
made money but it's over that time is
over I don't pay for records, I pay
to see the person, I pay for the
experience, I pay to see this
concert, I pay to feel Kendrick, I pay to
feel",
  "there's no sellout anymore people's like
there's no kids don't believe in sellout
anymore they're like ok this represents me on some level like
this like schoolboy q represents me on
this level and I'll wear a t-shirt
that's a schoolboy q because he
represents me his brand not his music",
  "I don't think it's different I think it's I think
it's the same like they're saying like
oh this is mine but like we live in a
time was like the thing was you bought
the record everybody bought thriller to
be like I bought thriller
but you don't own my own thriller",
  "People still do that for my
albums like it's an adult like I'm glad
people do but in the end it's the money
or like the entrepreneurships and
the brand it's not just
in the music anymore and it never really
was", 
  "like you think about thriller
the thing that made that pop
off are like Annie like Kanye or any
like Stevie like was like the ideas
behind it's like oh we have this like
that video is this short film he was the
first one",
  "if it was just the record it wouldn't be an
experience",
  "I wanted to come out at the right time it wasn't
supposed to come out then. I'm not
mad that people had it it's just like
because that's good I'm glad people are
like like if I like if I drop something
and people pick it up it's like oh
that's my bad for dropping it
I was mad more like my team for
releasing it like but I by accident but
I think like yeah music should be free
like I want people to pay for it
in different ways",
  "the thing is like music isn't really an experience
anymore unfortunately. it
can be but in general it's not like how
many times like I remember when I was in
college I was on every blog every day
reading about music reading music what
new band came out like back in the day
you just hear about a new band and then
you go see them and his experience and
like there's a movement ",
  "there's people still do it
but that's but it's less and less
because like you because music is by
itself",
  "It is but how many people like
have you ever been to like a listening
party, even a listening party do you
think like",
  "why not",
  "no yeah when you just sit down you to
the chill out and feel it
on a small level I feel like music doesn't
sound like it used to like it's all mp3s
and that doesn't sound is great we're
used to like shitty or quality sounding
stuff which is fine like you know that's
just how it is but humans are always
looking for the next thing we're always
looking for a bigger better thing like
that like we're always looking for more that's why you know well
that's what 'worldstar' about like I
love where we'll start because like you
see the most like either horrific a
great whatever like just more of it and
it's just that we're always looking for
that we can't live in a world where it's
just like we listen to the music and
that's the experience I'm like there's
gonna be it there's gonna be that
forever obviously just like there are
still plays but 14 year olds aren't
going to plays they're going to like
Katy Perry",
  "which is basically how play like
it's a big show like what's the
difference between that and like a big
Broadway article a big spectacle and and
they have their apps and they have their
phone",
  "yeah it is different but also it's like how many of
those kids are there because they're
like we love the sound of m.i.a and
how many of those kids are there because
they're like we know m.i.a is cool and
that represents me like it's like it
that represents me because like I knew a
lot of kids were like I love m.i.a and
they didn't really love m.i.a they
just knew it was the dope thing to like
and I mean it represented them as
a person",
  "Lynette is gonna have
to find artists like me like that's the
truth, they're gonna have to
because all bubbles pop. Everything in like
the universe will pop like all things
end. So it's like yeah this EDM things
like you need one person and play like
that I have no problem with that I
actually like those shows but that's my
kids are not gonna be like 'yo those shows
are the shit' ",
  "it's gonna end there's gonna be time like the kids
who like I was saying before we started
this like the kids who came and saw me
Thomas Lynette I've never heard anything
like that I was like really you never
heard voodoo? but like it's talking to
them on this level like this sound
sounds like trap to them there's a
really low and there's a really high and
there's a voice in the middle and that's
they know and that's all live it's like
it's all sickling musical music is
cyclical like it actually was talking a
family were like kind of it was funny
cuz I was like oh man like I'm around
the age now where I'm like everything
starting to come around again and I know
it all already
like what's popping in the UK I'm like
oh that's like 90s house", 
  "I just wanted to be honest I really wasn't
thinking I just I really wasn't this
wasn't like it like a plan I just like
you know let me just be honest. I
wrote down like everything I felt and
like I don't know I mean like I do
definitely like have bouts with things
like I don't really want to talk about
but like I go through it but
everyone does",
  "I realized that was already out there like I
realized like I mean I guess I could
hold it but like that's not how I live
my life like I was like everything I do
is on there already like people have
seen me change like people have seen
like and that's the thing everyone
does that. No one comes out of the
womb and it's just like this is what I
am forever. Everyone changes, you get
more experiences like I never left the
country like I just so I was like you
know I'm okay with people knowing that
much about me like and and I think it
also helps people like you know I have I
had kids come up to me be like you know
I read those and like it made me like
move to New York or like I read those
and like I came out to my parents like a
gret doesn't like and like that's cool
to me like if it gives somebody else
like the feeling of like oh like we're
all kind of lost here and it's okay like
that I'm cool with that",
  "it was really weird I felt really weird cuz I was the last
one that first of all I was like the
last one to shoot and if no one was in
the scene with me and it was like four
or six it was 6:00 in the morning snow
wasn't so I was by myself and a friend
of mine she'll be there
and then we got like food truck and
they got me like these gifts and it was
very teary and I and I could not cry I
couldn't shed tears.",
  "I just felt very like oh
man like oh this is like it felt like
you know like when your parents like
even drop you off at college or you
graduate you're just like 'so I'm not
going back there?' you know you're just
like you kind of it's it's just it just
it's a recurring theme that I feel like
is in everything in life where you just
like life is just like learning about
how to let go about stuff because like
you really can't enjoy things what they
are while you're in it like as soon as
you're in high school you're like oh I
get high school you're out of high
school and like once you're like in your
20s or like oh I get it I know it's
gonna get done it's all about the
experience so my experience there was
like you know unmatched like I couldn't
have asked for a better more talented
cast or writers or like damn like that
show like put me on like that show is
like so smart and and it was like the
heart of my fan base of like you know
kind of like people who felt like no one
really gets it and I really
appreciate but it was just like a weird
day like I get.",
  "all I could say was like
it was weird it was emotional",
  "Evette stayed Yvette stayed to the very
end and she like slept in her trailer I
was like you can go home she's like no I
want to be here like you know she's the
mom of the set so she wanted to be there
but everybody else like kind of home and
text messages and that's the way I want.",
  "we're all alone in the end so it's good", 
  "the way it has to be but that's okay you're
alone in the beginning too",
  "it's good that we're doing there's like we have this
interactions and then you come apart in
you come in like that's the way that
it's the way the universe is",
  "I learned last year the most important
thing and that is like you know the only
love that's reciprocal is love of self.
You really have to like yourself",
  "you can't rely on other people that's wack
it's so wack to be like please fill in
these holes in me that like I don't
think I'm strong and like that's wack
like you gotta like yourself enough to
be like no this idea is cool", 
  "that's stupid that's dumb
it should be like she's completely or
like this person completely like no he's
that person's a really good friend
sometimes I have sex with them it's
great but they are on this journey with
me and if they left it'd be okay.
you have to do that to yourself
otherwise you're just wasting"
)

# # # # Cleaning data for the new interview # # # #
#> Making text into a usable data frame
Current_interview_tibble <- tibble(line = 1:95, text = Current_interview)

#> Clean word frequency chart
ABC <- Current_interview_tibble %>%
  unnest_tokens(word, text) %>% # assigns individual words to numbers (nesting)
  count(word, sort = TRUE) # puts those words into a frequency graph

#> Word Count frequency
Current_final_dataset <- ABC %>%
  dplyr::filter(`n` >= 6, `n` <= 50) %>%
  dplyr::filter(`word` != 'in') %>% # removing filler words manually
  dplyr::filter(`word` != 'is') %>%
  dplyr::filter(`word` != "it's") %>% 
  dplyr::filter(`word` != 'but') %>% 
  dplyr::filter(`word` != "of") %>% 
  dplyr::filter(`word` != 'that') %>% 
  dplyr::filter(`word` != 'be') %>% 
  dplyr::filter(`word` != 'have') %>%
  dplyr::filter(`word` != 'like') %>% 
  dplyr::filter(`word` != "don't") %>% 
  dplyr::filter(`word` != 'for') %>% 
  dplyr::filter(`word` != 'not') %>%
  dplyr::filter(`word` != 'what') %>%
  dplyr::filter(`word` != "are") %>% 
  dplyr::filter(`word` != 'because') %>% 
  dplyr::filter(`word` != "with") %>% 
  dplyr::filter(`word` != 'on') %>% 
  dplyr::filter(`word` != 'they') %>%
  dplyr::filter(`word` != "i'm") %>%
  dplyr::filter(`word` != 'them') %>% 
  dplyr::filter(`word` != "this") %>% 
  dplyr::filter(`word` != 'so') %>% 
  dplyr::filter(`word` != 'if') %>%
  dplyr::filter(`word` != 'no') %>% 
  dplyr::filter(`word` != "yeah") %>% 
  dplyr::filter(`word` != 'at') %>% 
  dplyr::filter(`word` != 'something') %>%
  dplyr::filter(`word` != 'when') %>%
  dplyr::filter(`word` != "are") %>% 
  dplyr::filter(`word` != 'from')

# # # # Cleaning data for the old interview # # # #
#> Making text into a usable data.frame
Older_interview_tibble <- tibble(line = 1:84, text = Older_interview)

#> Removes filler words 
GHI <- Older_interview_tibble %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)

#> Word cloud frequency
Word1 <- Older_final_dataset %>%
  dplyr::filter(`n` >= 4) %>%
  dplyr::filter(`word` != "yeah") %>%
  dplyr::filter(`word` != "lot") %>%
  dplyr::filter(`word` != "blah") %>%
  dplyr::filter(`word` != "m.i.a") %>%
  dplyr::filter(`word` != "gonna") %>%
  dplyr::filter(`word` != "salty") %>%
  dplyr::filter(`word` != "olds")

# # # # Wordcloud graphing # # # #
#> Wordcloud for new interivew
wordcloud2(Current_final_dataset, size = 1.5, shape = 'circle')

#> Wordcloud for old interview
wordcloud2(Word1, size = 1.5, color= "random-dark", shape = 'circle')
