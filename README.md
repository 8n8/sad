# sad

**This is a work in progress, and doesn't work very well yet**

Sad is a method of writing Golang in a pure functional style and a source code analyser to enforce it.

Functions are divided into two kinds: pure and standard. Pure functions:

- cannot mutate their arguments
- cannot do IO
- cannot mutate variables, either global ones or those declared internally

So pure functions can only calculate a return value from their arguments, nothing else.

Standard functions are pre-defined, and will only be accepted if they are entered into the code exactly as pre-defined. They are used for IO and looping.
