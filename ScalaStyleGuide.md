#Scala Style Guide

####Authors: Vlad Patryshev

This document is supposed to be the result of common efforts to reflect what we feel good about by the end of year 2014. Things and views may change.
The document consists of two levels, one is for those who has just escaped from Javaland, and the next one is for those who can write code but at times makes mistakes.

Before applying any other rule, remember rule # 0: "The only true law is that which leads to freedom." (see other applicable rules in https://en.wikiquote.org/wiki/Jonathan_Livingston_Seagull)

##Level 1
This is for the people that never practiced functional programming in any language, but may be familiar with list comprehensions (like in Python) and functions like map, filter, fold. If not, there's plenty of books to read.

1.1. Scala is a mixed object-oriented and functional language. You are free to mix these two aspects as long as you can explain unto others how it works.

1.2. Ideally, your code should take care of all possible outcomes. Imagine, something throws an IndexOutOfBounds exception, and somewhere up the food chain some kind of client has to figure out who was it that went outside the range of its data, and why, and what can be done about it, except writing something to the log, so that admins send an email to a manager, and the managers bugs the developer, a day or two later. And you will be scratching your head: who was that bad guy that gave you an empty array when you so desperately needed the array's first element. Since you already know this can happen, you can add the code that takes care of this.

1.3. List of Do-nots:
- do not use nulls; nulls come from c, where it was a way to deal with monadic nature of calculations; Scala is a more advanced language. You can use `Option` or any other monad that helps deal with partial functions; the underlying argument is that not every type is an algebra over `Option` monad;
- do not use vars, except in a very narrow context where the var is visible in a very small scope and always has a certain value, like in 
```
   {
     var role = "Unknown"
     for (name <- listOfNames) { if (name == "Oedipus") role = "father killer" };
     println(s"found $role")
    }
```
- do not throw exceptions; exceptions in Scala are unchecked, and are used, rarely, for technical purposes. Business logic can do without exceptions, just admit that not every function is total, and accept the possible negative outcome, using, at least, `Option`, or better, like `Box` or `Result`.
  
- do not introduce setters and getters: setters are for remotely-controlled mutables by which you lose control over data - and there are better ways to deal with effects; getters are a stupid invention that fathers of Java cannot explain and were unable to exterminate (yes, I discussed it with Josh); you can use `def myValue = ...` to return `myValue`, a value of something inside your class, or you can use `lazy val myValue = ...` to cache the value if you want it evaluated just once.

- do not use mutables; they are like vars, but more sophisticated; for example, java.util.Date magically stores something inside, so one piece of code can instantiate it and then expect it being a certain value, but another piece of code can just change it without notice - this practice is now considered equivalent to quantum voodoo. How can you live without mutables? Cases vary. I guess we need to add more examples here.

- do not write methods more than 50 lines long (ideally, more than 14 lines long); there's refactoring available in IntelliJ that will allow you to _extract method_ (and it's your responsibility to give the method a good meaningful name).

- do not think this is java; learn to use pattern matching and lists and curried (partially evaluated) functions.

- do not hesitate to use more space characters than compiler wants you to; readability improves when your code looks like a front page of a newspaper, not the back page.

 - do not, except in rare occasions, use indexes of lists: even writing list(0) is bad, since we don't guarantee if the list has any elements. Here's a better way:
 ``` 
   val resul t= list match {
     case x::_ => Some(doSomething(x))
     case _    => None
    }
  ```
  Or another way:
  ```
    val result = list.headOption.map(doSomething)
  ```
  
- do not except for very technical reasons, use length of a collection. See, what kind of values would you be interested:
  - the length is 0 - this case may be a part of pattern match
  - the length is 1 - see above
  - the length is 2 - see above, `case x::y::Nil => doSomething(x,y)`
  - the length is odd or even - then, maybe, you need it.
  The problem is, calculating length of a lazy collection may take efforts, and may cause side effects, and what's the purpose of it?

1.4. The following are pairs of distinct entities:
- sets and lists: if you do not care about the order in which your elements are supplied, it's not a list, it's probably a set (except for the case when elements can encounter multiple times); so use a set. 
- order and sequence: in a list you specify in which order your elements are listed; ordered collection defines which element is less than another. Examples: The set of natural numbers is not a list, there's no way you can enumerate them. The list `List(1,3,2)` has three numbers enumerated, but this is not, generally speaking, the way you order these numbers (e.g. for sorting)
- maps and lists of pairs: a map is not a collection, whatever you were told by your teachers. A map is a partial function defined on a finite set of keys. It can be defined in a variety of ways, not necessarily by listing all key-value pairs. `OrderedMap` is a chimera: we take a map, but the set of keys is an ordered set.

1.5. The code you write will be read by compiler and by your colleague.
- To make compiler's life easy, specify the types of your values and functions; it's a known fact that scalac runs faster when types are specified.
- The same applies to your colleagues: if the type of a value is explicitly written (and not obvious otherwise), they'll feel more comfortable.

1.6. Introduce intermediate variables with good names: your compiler may need them, your colleague too.

1.7. Make methods do just one thing.

1.8. There are two kinds of zero-parameter "functions": those that just return something with no side effect, like `myInvestment.currentValue` and those that have a side effect, like `myInvestment.rebalance()`.

1.9. A function that returns anther function (e.g. `def add(x:Int)(y:Int):Int` is as good as `def add(x:Int,y:Int):Int`. Transformation of the second to the first called currying, and using this trick may also save your processor cycles (as if anybody cares these days).

1.10. Implicits. While you are at level 1, don't introduce new ones (it's tricky), but feel free to use whatever is available.

1.10. Your code is a piece of graphic art, although it's just an ASCII art; the more space characters, the more readable it is.

1.11. Max code line length... it's up to you, but remember, others are trying to read your masterpiece.

1.12. Nobody reads documentation or comments these days. So a self-explanatory code makes a lot of sense.

1.13. Within a project, it may make sense to use the established naming conveniences, coding style and libraries... on the other hand, if we did that, we would be still coding in machine codes and sitting around fire in the caves.

1.14. Learn local libraries.

1.15. If you don't know how to write unittests in Scala, drop everything and learn it first. Hardly any of your code makes any sense (even if you believe it does) if there are no tests that prove it (to others; we know you trust yourself). Test frameworks used may vary from project to project; it's a stupid idea to have two frameworks in one project.

1.16. DRY. Abstraction is your friend.

1.17. Read books on functional programming and advanced books on Scala.

## Level 2. You are an experienced Scala programmer, pretty versatile with stuff, can write typed and untyped SKI combinators (maybe not).

2.1. Keep in mind that level 1 programmers are reading your code: don't make their life harder than is necessary.

2.2. map/filter/flatMap are okay, but, on most occasions, the following code:
```
  for {user   <- db.readUsers
       kid    <- user.kids if user.gender == 'F
       school <- kid.school
       teacher <- school.teacherOf("math")
       phone   <- teacher.phoneNumber
      } yield call(phone)
```
is much more readable than
``` db.readUsers.filter(_.gender == 'F').flatMap(_.kids).flatMap(_.school).flatMap(_.teacherOf("math")).flatMap(_.phoneNUmber).foreach(_.call)
```
