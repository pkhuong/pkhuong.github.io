---
layout: post
title: "The logging pendulum"
date: 2020-11-15 14:21:21 -0500
comments: false
hidden: true
draft: true
categories: 
---

An application's logging development lifecycle usually goes something
like this:

1. Add enough log statements to convince yourself it works, and
   sprinkle some more to debug some early issues, if you don't have
   access to a real debugger.

2. Roll out to a less nimble environment: either someone else's
   computer, or hundreds of your own computers.
   
3. Something bad happens, and someone goes "if only we had logged $X."

4. Sprinkle more logging, which encourages people to insert their own
   logging statements when they add new code.

With experienced (scarred) people on the team, it's easy to leapfrog
any of the steps above.  The next step, however, is easier to avoid by
improving the expressivity of the runtime system than anything else.

5. Death by logging.  One bad situation became much worse because of
   logging overhead.  I'm not talking about "oh, there are too many
   useless lines here and there" because that's something you should
   trivially address in a log reader or `grep -v`.  I'm not really
   talking about effects like running out of disk (log rotation should
   be shared tech),[^distributed-log] but more about issues like "we
   didn't expect to call this code in a tight loop, but we do when
   $SITUATION and now we're spending 90% of our time formatting log
   records."

[^distributed-log]: As always, distributing the logging system so that logs have to go over the network adds more failure modes.

At this point, every new log statement becomes an exercise in
balancing imponderables: is this log line more likely to help me debug
a problem, or will it instead turn a bad situation into a catastrophe?

There's often no good answer to this question; not only because the
sufficiently smart developer is a myth, but also because a given piece
of logging may be invaluable in some situations, but often harmful
given their level of details.

In my experience, "just be smart" isn't a useful policy, and the end
result is that people instead stop thinking about logging... and the
application goes back to something like step 2 in the logging
lifecycle above.

How to stabilise the pendulum of verbosity
------------------------------------------

I know of two ways to stop swinging back and forth between
under-logging and over-logging.  In both cases, the key is to make the
downside of under- or over- logging (respectively) so small that we
can consistently err on one side.

Neither of these approaches solves the problem of knowing what to log
ahead of time.  That comes with operational experience; well placed
log statements are scar tissues.  However, they help us decide what to
do for new situations that have yet to make their imprint on the code.

We can address the reason for "over logging" in the first place (steps
2/3), and favour of a dynamic *deployment* environment, where the
operator is deeply familiar with the program, and empowered to hot
patch the code in production.  This would be an unsustainable
nightmare at scale, but not everything has to scale! In fact, even in
large-scale systems, there are pockets of small scale deployments; I
have worked on systems that were so efficient at doing a simple job
that they wouldn't need to scale past ~15 machines until the world
population hit ten billion people.

When we go "all-in" on nimbleness in production, we not only have
access to a REPL in production (e.g., a slime listener in common lisp,
or an ipython kernel), but, more importantly, the whole application
itself is designed for replacing bits of code on the fly.  For
example, any long-running loops will be written with the whole loop
body as a call to a toplevel function, with state passed around
explicitly as arguments and return values; this makes it easier to
hotpatch the loop body at runtime.

In that setting, a missing log statement can be inserted as soon as we
realise it might be useful.  This flexibility means we also don't rely
as strongly on predicting what information would be useful to log in
advance, a famously intractable problem.

By embracing late binding and dynamism in production, we have made it
easy to err on the side of underlogging, since a skilled operator can
always insert log calls as needed on the running system, and later go
through the regular process to hard code in the improved
observability.

Admittedly, small scale deployments with operators who deeply understand the
application aren't always feasible... but I think we also
[over-apply big computing patterns](https://hackernoon.com/big-and-small-computing-73dc49901b9a),
simply because a lot of developers were never exposed to anything
else.

Controlled dynamism in big computing
------------------------------------

When a small team is responsible for running your code on hundreds of
machines, or you claim it can be run on-premises by non-specialist
operators, a REPL and "just patch in the print statements you need"
isn't viable.

Injecting code at runtime definitely isn't enterprise... but disabling
pre-existing code is!

That's why we expect "production" logging frameworks to let us change
log levels with fine grained policy (e.g., verbosity levels per module
or file), and, most importantly, to make it easy to do so without
rebuilding the application.

Ideally, we can fine tune log levels on a running application, but
updating a config file and restarting the app is usually acceptable.

That's not enough to pin the logging pendulum in place: when we
prevent records from making it to logs, we must also be confident that
the performance impact of disabled log statements is very close to
rebuilding with the statement commented out.

Without such strong guarantees, disabling log statements differs only
marginally from filtering them out in a log viewer, and we will remain
wary of log statements taking down our code, even once disabled.

When we do have understandable guarantees on the performance overhead
of disabled log statements, we can more easily err on the side of
over-logging (perhaps at a level that's disabled by default), knowing
that the log statements are as good as commented out when we don't
want them, but can be re-activated when they would be useful.

We still have to guess what information would be useful to log, but at
least we don't have to also weigh the usefulness against the risk of
over-logging: we know that disabled log statements have a marginal
runtime impact.

I want to stress that we're not interested in the actual performance
of the application when it logs or doesn't log something, as much as
we are concerned with the *performance model of disabled log
statements*.  It's the performance model that lets us simplify the
decisions we have to make when writing the code... and I would thus
prioritise keeping the performance model simple and the worst-case
overhead low, even over improving empirical performance.

OK, so how far can we practically go down that route?
-----------------------------------------------------

We had [a simple runtime code toggling mechanism at AppNexus](https://github.com/appnexus/acf/blob/master/common/an_hook.h).
The direct overhead of a piece of code that's disabled by default is a
5-byte `TEST` instruction (a nop, practically), and additional code in
the "cold" section.

The direct overhead of a piece of code that's enabled by default and
is later disabled is an unconditional jump over the disabled code.

There are of course second order effects on code generation and memory
footprint, but those are negligible for most code... and for pieces
of code where that matters, developers should look at disassembly.

With the hooking mechanism, it was possible to toggle individual log
statements at runtime and in via a config file (for persistence). For
more common cases, one could enable with regexes to tweak log levels
per file.

Total amount of work, including the hooking code itself: < 1 week?

<p><hr style="width: 50%"></p>
