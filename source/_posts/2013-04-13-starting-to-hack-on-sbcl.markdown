---
layout: post
title: "Starting to hack on SBCL"
date: 2013-04-13 22:39
comments: true
categories: SBCL GSoC
---

SBCL was accepted as a mentoring organisation for Google's Summer of
Code 2013 (our list of project suggestions is
[here](http://www.sbcl.org/gsoc2013/ideas)).  This will be our first
time, so that's really great news.  I'm also extremely surprised by
the number of people who've expressed interest in working with us.  I
was going to reply to a bunch of emails individually, but I figure I
should also centralise some of the stuff here.

EDIT: There's a section with a bunch of general references.

EDIT 2: Added a note on genesis when playing with core formats.

Getting started
---------------

### Setting up the basic tools

The first step is probably to install git, and clone our repo
([the github mirror](https://github.com/sbcl/sbcl) works well, and
lets you fork to your own github account for easy publication).  Then,
building from source and installing SBCL (a local installation to
$HOME works fine) is obviously useful experience, and will be useful
to explore the source.  Reading INSTALL should be enough to get
started on Linux or OS X and x86[-64].  Other platforms may need more
work, and might not be the best choice if you're not interested in
improving the port itself (although I'm told FreeBSD and Solaris work
very well on x86[-64]).  To build SBCL from source, you'll need an
SBCL binary (bootstrapping from other CLs should work, but support
regularly bitrots away), and the usual C development tools
(e.g. build-essential on debian).  A fancy build (`./make.sh --fancy`)
is probably the best choice for development.

You'll also want to run the test suite often; better try it out now
(`cd tests; sh run-tests.sh`) to make sure you can get it working.
The test suite will barf if there's non-ASCII characters in the
environment.  [Oh my Zsh](https://github.com/robbyrussell/oh-my-zsh)'s
git customisation systematically trips me up, for example (I currently
kludge it and start a bash from ~ and then run the tests).

Once SBCL HEAD is working and installed, it's probably best to install
emacs and SLIME.  [Quicklisp](http://www.quicklisp.org/beta/)'s
quicklisp-slime-helper can take care of installing SLIME.  It is
possible to work on SBCL without SLIME.  However, SLIME has a lot of
useful extensions, if only to explore the code base.  If you're not
(and don't wish to become) comfortable with emacs, it's probably best
to nevertheless use emacs and SLIME for the REPL, debugger, inspector,
etc. and write code in your favourite editor.  Later on, it'll be
useful to figure out how to make SLIME connect to a freshly-built
SBCL.

### Exploring the source

I often see newcomers try to read the source like a book, and, once
they realise there's a lot of code, try to figure out a good order to
read the source.  I don't think that's the best approach.  SBCL is
pretty huge, and I doubt anyone ever simultaneously holds the complete
system in their head.
[RAM's "The Python Compiler for CMU Common Lisp"](http://www.cs.cmu.edu/~ram/pub/lfp.ps)
is still useful as an overview, and
[SBCL's internals manual](http://www.sbcl.org/sbcl-internals/) is a
good supplement.  Once you get close to bootstrapping logic,
[Christophe Rhodes's "SBCL: a Sanely-Bootstrappable Common Lisp"](http://www.doc.gold.ac.uk/~mas01cr/papers/s32008/sbcl.pdf)
helps understand the exclamation marks.  Past that, I believe it's
preferable to start out small, learn just enough to get the current
task done, and accept that some things just work, without asking how
(for now).

In that spirit, I'd say M-. (Alt period, Command period on some OS X
emacsen) is the best way to explore most of SBCL's source.  SBCL's
build process preserves a lot of source location information, and
M-. queries that information to jump to the definitions for any given
symbol (M-, will pop back up to the previous location).  For example,
if you type "(truncate" at the REPL and hit M-. (with the point on or
just after "truncate"), you'll find the out of line definition for
truncate in (mostly) regular Common Lisp, optimisation rules regarding
truncate, and VOPs, assembly language templates, for truncate called
with a few sets of argument and return types.  The out of line
definition isn't that interesting.  The transforms, however, are.
(VOPs aren't useful if one isn't comfortable with the platform's
assembly language, and mostly self-explanatory otherwise.)

The one to "convert integer division to multiplication" is a very good
example.  One could M-. on `deftransform`, and go down a very long
chain of definitions.  Instead, I think it's only essential to see
that the form defines a new rule, like a compiler macro, such that
compile-time values (lvars) that represent its two arguments are bound
to x and y, and the rule only triggers if its first argument is known
to be an unsigned word, and its second a constant unsigned word.  If
that's satisfied, the transformation still only triggers if the speed
optimisation quality is higher than both compilation speed and space
(code size).

Then, the constant value for y is extracted and bound to y, and a
conservative bound on the maximum value that x can take at runtime is
computed.  If truncate by y should be handled elsewhere, the transform
gives up.  Otherwise, it returns a form that will be wrapped in
`(lambda (x y) ...)` and spliced in the call, instead of truncate.

To extend SBCL's support for division by constants, it's not necessary
to understand more of SBCL's compiler than the above.  There's no need
to try and understand *how* deftransform works, only that it defines a
rule to simplify calls to truncate.  Similarly for lvar-value and
lvar-type: the former extracts the value for constant lvars, and the
latter the static type derived for that lvar (value at a program
point).  With time, knowledge will slowly accrete.  However it's
possible, if not best, to start hacking without understanding the
whole system.  This approach will lead to a bit of cargo culting, but
mentors and people on IRC will help make sure it doesn't do any harm,
and can explain more stuff if it's interesting or à propos.

### Finding where the compiler lives

Working on the compiler itself is a bit more work.  I think the best
approach is to go in `src/compiler/main.lisp` and look for
`compile-component`.  `ir1-phases` loops on a component and performs
high-level optimisations until fixpoint (or we get tired of waiting),
while `%compile-component` handles the conversion to IR2 and then to
machine code.  The compilation pipeline hasn't really changed since
the Python paper was written, and the subphases each have their own
function (and file).  M-. on stuff that sounds interesting is probably
the best approach at the IR2 level.

### Runtime stuff

The C and assembly runtime lives in `src/runtime/`.  There's a lot of
stuff that's symlinked or generated during the build, so it's probably
best to look at it after a successful build.  Sadly, we don't track
source locations there, but {c,e,whatever}tags works; so does grep.

GC stuff is in the obvious suspects (gc-common, gencgc, gc, etc.), but
may end up affecting core loading/saving (core, coreparse, save).
Depending on what in the core loading code is affected, code in
genesis (the initial bootstrap that reads fasl files from the cross
compiler and builds the initial core file) might also have to be
modified (mostly in `src/compiler/generic/genesis.lisp`).  That's…
more work.  Like the project suggestions list says, when we change
things in the runtime, it sometimes ends up affecting a lot of other
components.

GDB tends to be less than useful, because of the many tricks SBCL
plays on itself.  It's usually hard to beat pen, paper, and printf.
At least, rebuilding the C runtime is quick: if the feature
`:sb-after-xc-core` is enabled (which already happens for `--fancy`
builds), `slam.sh` should be able to rebuild only the C runtime, and
then continue the bootstrap with the rest of SBCL from the previous
build.  That mostly leaves PCL to build, so the whole thing should
takes less than a minute on a decent machine.

Some references
---------------

I was replying to an email when I realised that some general compiler
references would be useful, in addition to project- and SBCL- specific
tips.

Christian Queinnec's
[Lisp in Small Pieces](http://pagesperso-systeme.lip6.fr/Christian.Queinnec/WWW/LiSP.html)
gives a good overview of issues regarding compiling Lisp-like
languages.  Andrew Appel's
[Modern Compiler Implementation in ML](http://www.cs.princeton.edu/~appel/modern/ml/)
is more, well, modern (I hear the versions in C and Java have the same
text, but the code isn't as nice… and ML is a very nice language for
writing compilers).  I also remember liking Appel's
[Compiling with Continuations](http://www.amazon.ca/Compiling-Continuations-Andrew-W-Appel/dp/052103311X),
but I don't know if it's particularly useful for CL or the projects we
suggest.

For more complicated stuff, I believe Stephen Muchnick's
[Advanced Compiler Design and Implementation](http://www.amazon.com/Advanced-Compiler-Design-Implementation-Muchnick/dp/1558603204)
would have been really nice to have, instead of slogging through code
and dozens of papers.  Allen & Kennedy's
[Optimizing Compilers for Modern Architectures: A Dependence-based Approach](http://www.amazon.ca/Optimizing-Compilers-Modern-Architectures-Dependence-based/dp/1558602860)
is another really good read, but I'm not sure how useful it would be
when working on SBCL: we still have a lot of work to do before
reaching for the really sophisticated stuff (and what sophistication
there is is fairly non-standard).

I believe the Rabbit and Orbit compilers have influenced the design of
CMUCL and SBCL.  The
[Lambda papers](http://library.readscheme.org/page1.html) provide some
historical perspective, and the RABBIT and ORBIT theses are linked
[here](http://library.readscheme.org/page8.html). 

What little magic remains in SBCL and CMUCL is the type derivation
(constraint propagation) pass, and how it's used to exploit a
repository of source-to-source transformations (deftransforms).  The
rest is bog-standard tech from the 70s or 80s.  When trying to
understand SBCL's type derivation pass at a very high level, I
remember finding Henry Baker's
[The Nimble Type Inferencer for Common Lisp-84](http://home.pipeline.com/~hbaker1/TInference.html)
very useful, even though it describes a scheme that doesn't quite work
for Common Lisp (it's very hard to propagate information backward
while respecting the final standard).  Kaplan and Ullman's
[A Scheme for the Automatic Inference of Variable Types](http://pdf.aminer.org/000/546/423/a_general_scheme_for_the_automatic_inference_of_variable_types.pdf)
was also helpful.

Getting help
------------

Over the years, I've seen a couple of people come in with great
ambition, and give up after some time, seemingly without having made
any progress.  I believe a large part of the problem is that they
tried to understand all of SBCL instead of just learning the bare
minimum to get hacking, and that their goal was too big.  I already
wrote that SBCL is probably best approached bit by bit, with some
guidance from people who've been there before, and I hope the projects
we suggest can all lead to visible progress quickly, after a couple
days or two weeks of work at most.

Still, before investing my time, I like to see the other person also
give some of theirs to SBCL.  This is why, as I wrote on the mailing
list last week, I'm much more inclined to help someone who's already
built SBCL on their own and has submitted a patch that's been
committed or is being improved on the mailing list.  I absolutely do
not care what the patch is; it can be new code, a bugfix for a highly
unlikely corner case, better documentation, or spelling and grammar
corrections in comments.  The bugs
[tagged as easy in our bugtracker](https://bugs.launchpad.net/sbcl/+bugs?field.tag=easy)
may provide some inspiration.  However trivial a patch might seem,
it's still a sign that someone is willing to put the work in to
concretely make SBCL better, and I like that… it's also a minimal test
to make sure the person is able to work with our toolchain.  (This
isn't SBCL policy for GSoC.  It's simply how I feel about these
things.)

Again, I'm amazed by the number of people who wish to hack on SBCL
this summer (as part of Google's Summer of Code or otherwise).
Because of that, I think it's important to note that this is our first
year, and so we'll likely not have more than two or three spots.
However, I always like seeing more contributors, and I hope anyone
who'd like to contribute will always be guided, GSoC or not.

Finally, I'll note that Google's Summer of Code program was only a
good excuse to write up our
[list of projects](http://www.sbcl.org/gsoc2013/ideas/): they're
simply suggestions to incite programmers to see what they can do that
is useful for SBCL and, most importantly, is interesting for them.
Anyone should feel welcome to work on any of these projects, even if
they're not eligible or chosen for GSoC.  They're also only
suggestions; if someone has their own idea, we can likely help them
out just the same.
