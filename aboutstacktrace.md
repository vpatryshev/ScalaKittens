# Architecture Sample via Java Stacktrace Analysis 

Once I went to my friend's website and tried to open a page... and see what I got:

```
org.firebirdsql.jdbc.FBSQLException: Resource Exception. Unable to complete network request to host "".
Reason: Unable to complete network request to host "".
	at org.firebirdsql.jdbc.AbstractConnection.ensureInTransaction(AbstractConnection.java:842)
	at org.firebirdsql.jdbc.AbstractStatement.executeQuery(AbstractStatement.java:155)
	at helpers.ExtFunc.GetObjectId(ExtFunc.java:53)
	at BaseImpl.Init(BaseImpl.java:51)
	at Catalog$Impl.process(Catalog.java:325)
	at Catalog.service(Catalog.java:389)
	at sasta.ServletBase.service(ServletBase.java:136)
	at javax.servlet.http.HttpServlet.service(HttpServlet.java:588)
	at org.apache.jserv.JServConnection.processRequest(JServConnection.java:317)
	at org.apache.jserv.JServConnection.run(JServConnection.java:188)
	at java.lang.Thread.run(Thread.java:534)
at org.firebirdsql.gds.GDSException: Unable to complete network request to host "".
	at org.firebirdsql.jgds.GDS_Impl.isc_start_transaction(GDS_Impl.java:555)
	at org.firebirdsql.jca.FBManagedConnectionFactory.getCurrentIscTrHandle(FBManagedConnectionFactory.java:507)
	at org.firebirdsql.jca.FBManagedConnection.findIscTrHandle(FBManagedConnection.java:1125)
	at org.firebirdsql.jca.FBManagedConnection.internalStart(FBManagedConnection.java:675)
	at org.firebirdsql.jca.FBLocalTransaction.internalBegin(FBLocalTransaction.java:111)
	at org.firebirdsql.jca.FBLocalTransaction.begin(FBLocalTransaction.java:94)
	at org.firebirdsql.jdbc.AbstractConnection.ensureInTransaction(AbstractConnection.java:838)
	at org.firebirdsql.jdbc.AbstractStatement.executeQuery(AbstractStatement.java:155)
	at helpers.ExtFunc.GetObjectId(ExtFunc.java:53)
	at BaseImpl.Init(BaseImpl.java:51)
	at Catalog$Impl.process(Catalog.java:325)
	at Catalog.service(Catalog.java:389)
	at sasta.ServletBase.service(ServletBase.java:136)
	at javax.servlet.http.HttpServlet.service(HttpServlet.java:588)
	at org.apache.jserv.JServConnection.processRequest(JServConnection.java:317)
	at org.apache.jserv.JServConnection.run(JServConnection.java:188)
	at java.lang.Thread.run(Thread.java:534)
```

## So, What happened here:

- Apache web server gets a request, and calls a service
- The service calls someone named `sasta`
- Sasta starts looking up for a service in a catalog
- the catalog has a class which name ends with `Impl`, which means this is a Working Class, and he tells the worker: work!
- this poor `Impl`, he won't work until his Base Class, `BaseImpl` (a Base Worker) is not *initiated*
- now our Base Worker has no choice but to start doing something; here what he does:
  - it has `Helpers`, and it calls its helpers, which are not simple guys, they have *functions*, `ExtFunc`
  - Base Worker asks his Helpers Functions to tell him *Object Identifier*
  - the Functions, judging by their naming style, were written by a runaway c# programmer
  - to learn the *Object Identifier* Helpers had to go to the Database
  - to talk to the Database, they had to concoct a spell in the *database language* (which is SQL)
  - Helpers now say this spell and they want a *Firebird* to do some magic
    - now the Firebird starts thinking...
    - even with the magic *sql spell* it cannot tell the Object Identifier except in a *Transaction*
    - so the Firebird tries to open a Transaction
    - where's a Transaction, there's a *Security*, NSA, Java Cryptography Architecture; yes, a Firbird can talk to NSA
      - NSA Cryptographers allow to start a Transaction, actually they start it themselves
      - to do that, Cryptographers first have to start an *Internal Transaction*
      - but since they cannot start an Internal Transaction without notifying their management,
      - Cryptographers dare to bother their *Management*
        - Cryptographers Manager is actually a nice guy, and he starts an internal operation, *internalStart*
        - but he is a Manager! He cannot just do the job without checking whether maybe somebody else already did it
        - so the Manager starts looking around for someone who already did it before
        - he wants to start an operation called `findIscTrHandle`
        - being a Manager, he cannot do two *action items* in one day (or else he would have been an Engineer)
        - so the Manager wants to know what are his Cryptographers doing right now
        - and he starts the operation called `getCurrentIscTrHandle`
        - the Manager does not do this operation himself, but sends the request to the people responsible for this kind of job
          - the Responsible Ones, they get an *action item* to do
          - so they start a Transaction, via `isc_start_transaction`
          - they do it in a separate *Thread*, because they are some kind of independent contractors
          - in this new Thread, they are trying to figure out, what exactly should they do?
          - and while discussing this, they figure out that the name of the *Request Owner* is empty. An empty string. Just imagine if your name were "". 
          That would have caused a lot of fun in an "Information System". Our system, in our company, until recently, did not even accept two-letter names,
          like, say "He", "Mo", "Li", or even worse, "Ng".
          - so the Contractors just laugh at the Manager, telling him to *buzz off*
        - the Manager, having got this refusal, tells his Cryptographers to *buzz off*
      - Cryptographers tell the Helpers to *buzz off*
    - Helpers tell the Base Worker to *buzz off*
  - Base Worker does not bother the SubWorker, and tells the Service to *buzz off*
- and the Server publishes all this bureaucracy, because *your name string is empty"

Wha?

That's Java architecture in its typical implementation.

