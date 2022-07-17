# sad

Sad is a method of writing Golang in a pure functional style and a source code analyser to enforce it.

Functions are divided into two kinds: pure and standard. Pure functions:

- cannot mutate their arguments
- cannot do IO
- cannot mutate variables, either global ones or those declared internally

So pure functions can only calculate a return value from 
