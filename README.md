#Scala Kittens Project#

*Attention Please*
This project is currently split into two versions: scala210 and scala211. Libraries are different in these two versions of Scala.

In this project I store small solutions to typical problems, which are free to grab and too small for building a "library".
[Scala kittens][slides] has examples of this kind of stuff, one-liners. Probably they deserve to be put to gist.

Currently there are Caching, FS, OS, ClientHttpRequest, Result and some samples.

##Result##

Presentation here: https://docs.google.com/presentation/d/11Yzt-VvL5emFHmG1ku4QVNQbo7LawC8qinDX1bN3ifY/edit?usp=sharing

This is my version of scalaz.Validating; somewhat less highbrow applicative monadic monoidal bs
(but yes, it is a monad, and it is an applicative functor).

`Result[T]` represents a storage for results of unreliable calculations, with possible list of errors produced during the process.
`Good[T]` is how good results are stored; `Bad[T]` is how bad results are stored (this is just a list of error messages, with proper functionality);
`Empty` is a weird case when we do not have any results at all, like `None` in `Option[T]`.
One can chain, blend, check the results; all that you can do with `Option` you can do with `Result`; one can `fold` it like they do with `Either`.
Using `Result` in sugared loops (aka "list comprehensions") makes the loops even sweeter.

##Caching##

Implements single value caching strategy.

###Purpose###

Why would we need to cache just one value? There are a couple cases.

1. You have some specific entity that has a limited validity time, and is a little bit expensive to reevaluate on each call. `lazy` won't work, since it evaluates once. So there.
   For example, a map of phone country codes; there's just one such list, but it changes from time to time.
2. You don't care about memory, and you'd be willing to store your database table in a `Map`, but individual
   values still may expire and require reevaluation.
3. You want unused data to silently disappear from memory. You can do it by applying a reference selector, like
   `withSoftReferences`, `withWeakReferences`, `withHardReferences`, so that the value can be discarded according
   to the appropriate reference policies. E.g. if you use `withSoftReferences`, the value is discarded
   if there's not enough memory. This discarding policy has String to do with the data validity, though.
   Using this kind of discardable references has some negative impact on performance in the tests, but this is a non-issue: if we are saving
   on seconds, losing on milliseconds should not bother us.

###How to use###

Here's an example

    val hourNow = cache(() => new java.util.Date().getHours).withSoftReferences.validFor(60).MINUTES

We provide a function that produces a value; this function's return type is the type of the value cached
We specify that this value gets invalidated after 60 minutes
To use the value, we can write

    val theHour: Int = hourNow()

###Methods###

Import `Caching._`, and use `cache(function)` to specify an instance of cached value; `validFor(timeout: Long)`,
followed by one of `NANOSECONDS`, `MILLISECONDS`... `DAY`.

The resulting instance has just one public method, `apply`, which can be called by parentheses.

###How It Works#

Let's say we do it `withSoftReferences`.

When we specify evaluator function and validity period, these are stored in an instance of `Container` that is pointed to
by `SoftReference` stored in `AtomicReference` within `CacheInstance`. `CacheInstance` is what is returned
by `cache`. The moment you invoke `apply` method, the value starts evaluating. That's if you call it first time.
While it is evaluating, other threads can come for the value; they'll have to wait, since the value is stored in a `lazy val`.

Once the value is evaluated all potential consumers receive it.

Now the value may be invalidated for two reasons. Either it's expired, by timeout, or garbage-collected for the lack of memory,
since it is pointed to by a `SoftReference`. In the latter case, it's collected; in the former case it stays until
someone comes for the value. At this moment, the invalid value is discarded, and a new `Container` is created.
We try to store it into our `AtomicReference` using `CAS`; if we succeed, this is the new container. Otherwise,
we do not worry: some other thread did it for us. Anyway, next we try to extract a value.

A loop may ensue, because the value we think we have may as well be garbage-collected while we were retrieving it.
Bad luck, try again.

So that's how it works. If you look at the test, you'll see a couple of examples. The second example runs 20000 threads
for about 20 seconds, controlling their activity, making sure that all 5000 burst at once each cycle, competing for the access to
one single cache value; the test check that they do not recalculate the value until the time is up.

Credits: [lj user="sassa_nf"](sassa_nf.livejournal.com) [discussion](http://ivan-gandhi.livejournal.com/1974346.html)

[slides]: (https://docs.google.com/present/view?id=dc7rg5cv_76d7dpx5c5&revision=_latest&start=0&theme=blank&cwj=true&pli=1)

##FS (Flie System)##

FS provides shortcuts for typical operations with files and directories, with less hassle than usual.

    folder(".").file("mytext.txt").text = "this is the contents of my file"

will write the contents to the file. There's a bunch of such one-liners.

##ClientHttpRequest##

This class comes from my old Java code on myjavatools.com

Pls refer to the test to see how to use it. It's easy actually.
Note that in your web app you can also trace the progress. Might be convenient.
And you can cancel it half way through.

## Scala Versions ##

Currently the base version was moved to scala210 directory, since it targets scala 2.10. A version for Scala 2.11, when finished, will be deployed in scala211 directory.

