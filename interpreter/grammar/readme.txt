Description of language
=============================
Grammar is strongly based on C.

Operators have different priorities. && i || have the lowest priority,
then there are comparators, the rest is pretty obvious.

Execution of program begins in main() function. Main doesn't take any
arguments, but it can return values of any type. Interpreter ignores returned
value. If main has incorrect definition (eg. takes any arguments) or its
definition is missing, then interpreter returns with error.

There are 3 types: int, bool, string. They can be mixed with tables and
dictionaries. Tables are indexed like in C, beginning from 0. Tables have
constant size specified during declaration.

One should declare variables, and then use them. You can't declare and
assign variable in one statement. During declaration variables are assigned
the following default values:
  int        - 0
  bool       - false
  string     - ""
  table      - defauld value of holded type
  dictionary - empty at the beginning
You can't specify your own value during declaration. In the current scope
you can declare one variable multiple times, new declaration hides the old
one. You can also mask all functions with this mechanism (even those embedded
and shipped with interpreter ie. tostr, toint). Each block {...} introduce
new scope - like in C++.

You can use default tostr(int i) and toint(string s) functions to convert
between strings and ints. If convertion is impossible it causes runtime
error.

Keyword 'print ...' prints contents of any variable, even tabular or
dictionary.

Arguments are passed to functions by value. There is no void type, functions
always returns values. If there is no return statement in the body of a
function default value of expected type is returned. Functions support
recursion. Each declared function is visible before execution of program.

for-loop in pascal style declares int variable inside loop body. Computation
and assignment of starting and ending values happens before execution of
loop and is done only once. Inside loop body counter can be modified, but
its value is restored and set after each run.

Errors such as division by 0, access to invalid element of an array or
dictionary are handled during runtime and causes stop of execution. Static
type check is done during program loading, before execution.
Interpreter checks:
 - number and types of function parameters
 - correctness of types in expressions (method bottom - up)
 - correctness of types in assignments
 - correctness of types in indexing of tables and dictionaries
 - returned values
 - types of expressions in for loops - they must be ints
Arguments and returned values for operators (types in operands have to match):
 && ||        takes bool;               gives bool
 >= > <= <    takes int;                gives bool
 != ==        takes int, string, bool;  gives bool
 +            takes int, string;        gives int, string (concatenation)
 - * /        takes int;                gives int

Please see grammar (lang.cf file) and examples for further clarification.

Comments
=============================
C style: //... and /* ... */

Grading
=============================
I will implement elements 1-5, 6b, 6c in section for 16 points,
additionally 1-4, 5b, 5c in section for 20 pts.

I expect that when summed up, features of my language will give about
20 pts for this project.
