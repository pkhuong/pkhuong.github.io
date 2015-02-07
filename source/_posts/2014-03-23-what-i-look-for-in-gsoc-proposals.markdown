---
layout: post
title: "What I look for in GSoC proposals"
date: 2014-03-23 15:58
comments: true
categories: 
---

EDIT: Added a link to a [solid proposal from last year (PDF)](/images/2014-03-23-what-i-look-for-in-gsoc-proposals/regalloc-abarch.pdf).

It's already
[Google Summer of Code](https://www.google-melange.com/gsoc/homepage/google/gsoc2014)
season; like spring, it's slightly earlier this year.  For multiple
reasons -- including the accelerated schedule and my transition to
having a Real Job -- I did not manage to guide any of the students who
sent in applications.  After an initial ranking, we're left with a
fair number of decent proposals for a Summer of
[SBCL](http://sbcl.org/).  They're of comparable quality, and all
leave me a bit uneasy.  In this post, I'll try to explain what I look
for in GSoC proposals… or, really, in action plans in general.

I care about the quality of GSoC plans for two reasons.  First,
because it reassures me that the applicant understands the
difficulties they'll have to overcome to complete their project.
Second, because, like TDD, a good plan makes it easier to feel when
we're making concrete progress.  To me, these two factors are
necessary conditions for a good success rate.

We'll probably allow additional time to update the proposals we have
already received, so hopefully this post can be immediately useful.  I
have a single vote, and other mentors no doubt have different
criteria.  I only hope that this post can give ideas to applicants
looking to review their proposal.

Why? Software development can't be planned!
===========================================

This is a common objection to any form of planning for software
projects.  I don't understand where it comes from.

Software development should be *easier* to plan than most endeavours:
we have to deal with relatively few people, and, when we do, it's
rarely in an adversarial setting… and I'm willing to bet most of us
can better forecast the behaviour of programs than that of people.
There's currently an electoral campaign in Québec, and I'm certain
that, before the date was even announced, every single party had a
precise plan for each day, at the riding, regional, and provincial
levels.  Politicians can do it; why can't software developers?

Of course, there will be constant adjustments to the initial plan.
The reason I think it's important to spend time and thought on that
plan isn't to stick to it, but primarily because it forces us to
systematically consider all sorts of potential issues, challenges,
failure modes, etc.  This exercise becomes useful when problems crop
up: rather than making decisions in the heat of the moment, we can
rely on our past (well rested and composed) self's research, notes, and
ideas.

I also find plans tend to become much more handwavy as we get farther
in the future.  That's a normal tendency.  I think we're usually
pretty good at short-term planning; most people can cook a meal
without sitting down to figure out how they'll cause awesome chemical
reactions to occur without burning their house down.  It's a different
matter when it comes to month-long camping trips or feeding a thousand
people.

The difference is one of scale, and it's exactly when we're out of our
comfort zone that mindful planning becomes useful.  I expect an action
plan to be as thorough in its last week as in its first.  Most of it
will be fiction, but, as I wrote above, the thinking involved in
writing that fiction is what I'm after.

The overall structure I'm used to
=================================

There are tons of ways to describe a plan.  I like this one, but the
main thing is what I have in mind when I read each section.  For
instance, [last year's proposal for register allocation via graph colouring (PDF)](/images/2014-03-23-what-i-look-for-in-gsoc-proposals/regalloc-abarch.pdf) had:

1. the project's aim, with a description of the high level goal (better
register allocation) and of mechanisms to implement (new allocation
heuristic, live range splitting and coalescing, etc.);
2. a plan section, describing each proposed step (find examples of bad
allocation, write a new simple allocator, write a visualisation tool,
…) in details;
3. a schedule at a 2-week granularity, with references to the detailed
descriptions and deliverables for each step;
4. a list of risks and challenges in the project;
5. a short cv;
6. why the student felt motivated and suitable for the project.

It's a different structure than what I suggest below, but both mostly
present the same information.  My wish is to feel confident that the
student has done the work to have that information.

First, a description of the goal.  SBCL offers
[prefab proposals](http://www.sbcl.org/gsoc2014/ideas/), and it's
tempting to just copy-paste.  However, I feel much more confident in
the intrinsic motivation of a student who's made the project their
own.  Many of our suggestions are skeletons for projects; it should be
easy to come up with ways to extend and better specify them according
to the student's interests.  Others are already fleshed out; those
are a bit harder, but even small variations on the initial theme
are encouraging.  In all cases, if I don't feel like the student owns
the project, I probably won't rank the proposal highly, and certainly
won't be inclined to mentor them.  The problem statement is a simple
way to demonstrate one's understanding of -- and interest for -- the
project.

Second, a review of the current environment.  What currently exists,
in SBCL or elsewhere? Can we exploit interesting research or code?
Has there been previous efforts, and how/why did they not pan out?
Perhaps there's a relevant post-mortem from a similar project that
will help understand the problem domain.  What potential difficulties
do we know of?  What don't we know (known unknowns)?  How bad do we
estimate unknown unknowns to be, and why?  What persons or resources
could be helpful?  To a certain extent, this is simply demonstrating
that one's done their due diligence.  However, it also tells other
people (e.g., potential mentors) how they can best work with the
applicant.  This section should be a good resource to re-read when bad
things happen.

Third, the suggested course of action at a high level.  Not only what
would happen in a perfect execution, but, ideally, ways to address (ahead
of time or when/if they occur) some of the difficulties and unknowns
listed in the previous section.

Fourth, success indicators, a translation of the initial goal
description into a few checkboxes.  Ideally, the criteria are
falsifiable… much like an experiment, with the null hypothesis being
project failure.  I find that checking even a few of these boxes gives
me a good sense of closure on a project.

Finally the calendar, to show how one might execute the third section
and satisfy the success indicators in the fourth section.  The first
few weeks are usually easy to envision, but proposals tend to become
fuzzier with time.  I'm more confident in proposals that only use one
or two -week periods, with *actions* and a few testable for each
period.  Shorter periods make fuzziness more obvious, but, more
importantly, they combine with the milestones to let us tell that
we're (or aren't) getting traction.

In the unfortunate case that we're somehow not making the expected
progress, frequent milestones means we can tell more quickly that
something is off.  We only have to make up for a one week setback
rather than for a month, i.e., one quarter of the GSoC program.  The
odds of outright project failure are lower, and we don't risk feeling
(as) down for wasting a month of work.

One thing that I find particularly scary in project calendars are
"Magic Happens" steps.  This seems more common with longer planning
periods.  Many proposals are quite detailed with respect to
preliminary work and research, and to final integration and
documentation.  The issue is that most of the work is stashed in a 3-5
week interval during which the actual project becomes completed.  It's
not clear what the student will do to get there (or even who will do
the work ;).  Planning each week individually makes such steps
obvious.

I know that it's hard to determine how we'll achieve a goal when
we've never done something similar before.  But we can pretend.  I
want to feel that an applicant has a clear idea how to execute the
task they propose, if only in an artificial perfect world.  Otherwise,
why should I believe they can do so in reality?

Planning is fiction
===================

That's it. I'm told that writers should show rather than tell.  I feel
the same about plans.  I like to be shown how goals could be achieved
in ideal and less-than-ideal worlds rather than told when steps will
be completed.  I think the most important point is that I don't
appreciate detailed plans because we should (or can) stick to them.
Rather, I believe that the exercise of coming up with such a plan is
an efficient way to ensure we can fruitfully respond to what'll
actually happen when we execute it.

The regalloc proposal was one of the longest ones last year, but I
felt confident that the student had a good idea of what challenges
made the task non-trivial.  In the end, we followed only a small part
of the plan: it's hard to come up with a suite of bad regalloc
examples, visualisation turned out not to scale to real code, and
there was too little time to work on live range splitting.  However,
every time there was a change, Alexandra had no problem determining
how to adjust the schedule and figuring out the next step.  On my end,
I could easily follow the modified schedule and see that we were
making progress.

This planning exercise is not easy, and it's certainly not quick.
(FWIW, I find it useful to get someone else, even without relevant
experience, to look at the plan and ask questions.)  Action plans have
helped me countless times: when I needed to react to problems; when I
felt like I was running in circles but could tell *for sure* that I
was making progress; when a project forcibly came to an end but I
still had a sense of closure… Hopefully, they can do the same for SBCL
GSoC students.
