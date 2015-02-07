---
layout: post
title: "Non-blocking garbage collection: mostly an exercise in page flipping"
date: 2014-08-26 22:30
comments: true
categories: 
published: false
---

My work at [AppNexus](http://www.appnexus.com) sometimes feels like
we're slowly developing the runtime system for another instance of
[Greenspun's tenth law of programming]()… except that I don't know of
any Lisp or functional language implementation with a comparable level
of support for lock-free or non-blocking operations (some specialised
JVMs come close).  A big part of that impression stems from our use of
[Safe Memory Reclamation]() to help us implement lock-free operations
in C.

Perhaps that's why proponents of 

