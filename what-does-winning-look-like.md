# What does winning look like

> I had the pleasure of a long chats with a (ex) lieutenant general of logistics from the british army.
> We spent some time talking about 'getting things done' in our respective fields and the phrase I enjoyed most
> from that was 'define what winning looks like'. Without agreement on that, he said, there is a very low
> chance of success as how would you know if you achieved it even if you did.

Allow the user to easily produce named data stores of flat data which can be processed more quickly than would
otherwise be easy in Common Lisp.

The system must have a set of restrictions which allow the user to produce code that can potentially take
advantage of high performance features of the users computer.

The system data stores and processing functions must be able to be redefined or restructured live in a fashion
that fits with usual Common Lisp programming.

It should be clear made that many cases do not fit this system and should not be attempted.

The system must be the most minimal system that allows the following user stories to be possible:


## S0

A user opens a game (or other realtime project) using Tables (henceforth referred to as the 'system')
and starts it running.

They remove a seemingly useless column from a table, compiles it and the system informs then it would break 2
queries.

The user sees that the first query can be trivially updated, does this, recompiles and continues

the second query is less obvious. The user calls a function to trace the query and is informed of:
- the table & columns directly affected
- the queries that take those columns as an input

The user calls the trace function again but with a number to indicate how far to recur the search.

The user see the change that needs to be made to untangle the connections.

After fixing the respective queries, the user removes the initial column they were interested in and recompiles.
The system accepts and makes the change.

# S1

A user wishes to improve their understanding of the performance of the system.

They call a function to instrument all queries passing in a function of their own

Their function has a bug and crashes.

The error is caught, their instrumentation removed, the user is informed and the system continues as usual.

The user fixes their function and instruments the system again.

Their function is now called at the points in execution specified by the feature.

With this data the user identifies a particular query they wish to profile.

The user copies & pastes the definition table and renames it.

They call snapshot which is a system feature which duplicates all data in a table if a layout matches (within
certain parameters). It avoids having to define a new query separately with the sole purpose of copying a table.

They copy the query they are interested in testing and rename that.

They then call a function to run the query on the table a certain number of times.

The system runs it once without recording which allocates enough memory for the output and then runs the
query *n* times, calling the user's instrument function as required.

The api contains functions that perform the same task as changing the code, in this case they make a script
to redefine a table and testing to see how it affects performance.

The data allows the user to improve the performance of the query.

# S2

As the user's data layouts get finalized the user adds :static to the tables and queries to indicate that
they will not be changed. This allows the system to optimize the generated lisp a little further

The user wants to ship and no longer needs live recompilation. They add :static to the define-system in their
project and recompile.
