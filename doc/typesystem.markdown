# Eve's Typesystem

Eve is statically, strongly, latently, structurally typed.  This means:

* **static**: Every expression in the language has a type.  Types are a compile-time property of the program, not a run-time property of a particular execution.
* **strong**: Type errors are either caught at compile-time or result in a well-defined runtime exception.  They do not lead to undefined behavior.
* **latent**: You do not need to write type declarations directly in source code; the compiler infers them.
* **structural**: Types are checked based on their structure, not on their name.  Type names are just synonyms for easy labelling, and are not required for typechecking.  (Note: [union](#union_types) and [opaque](#opaque_types) types are exceptions, where the types must be named in order for the type inferencer to work).

Unlike in several other programming languages, the primary purpose of Eve's type system is not to prevent programming errors.  Rather, it is a documentation tool for the maintenance programmer or library user.  By using a type inference algorithm, EveDoc can automatically determine the types of functions without the programmer specifying them by hand.

## Types

Types are sets of values.  When an expression has a certain type, it means that that expression may only evaluate to values in that type.

Each value belongs to only one type.  There is no subtyping in Eve; you cannot say that a value is-a CheckingAccount, Account, FinancialInstrument, and so on up a hierarchy.  However, there is an [interface](#interfaces) mechanism that lets you define expressions that may hold any value that satisfies a certain set of constraints.

You may also use type variables when writing out compound types.  These can stand for any type, and give you what's known as "generics" in C++/Java land or "parametric polymorphism" in Haskell/ML land.  For example, the type signature `(a -> b, List<a>) -> List<b>` denotes a function that takes another function (from anything to anything) and a list that corresponds to the arguments of that function, and then returns a list of the return types.  Type variables should always have a lowercase first letter.

There is an infix keyword `as` that you can use for explicit type declarations:

    2 as Int
    binary_operation as (Int, Int) -> Int
    map as (a -> b, List<a>) -> List<b>
    find as (a, List<a>) -> Int where Eq(a)
    (2 + 4) as Int

Note that `as` is a very low-precedence operator: if you want the type declaration to apply to a compound expression, as in the last example, you should parenthesize the expression.

The individual arguments and return type of a def may also be specified inline.  This declaration:

    def map(fn as arg -> retval, elements as List<arg>) -> List<retval>:

is exactly the same as this one:

    def map(fn, elements) as (arg -> retval, List<arg>) -> List<retval>:

The first alternative is usually preferred when the argument types are fairly complex in their own right, while the second is preferred when the full function signature is simple.  The first alternative also lets you omit parts of the type declaration: if you wanted to omit some of the argument types or the return type, the type inferencer would fill them in for you, checking that any remaining declarations match the inferred type.

You can define type synonyms with the `typedef` declaration:

    typedef Point: { x: Int, y: Int }

This defines a name that you can use to refer to a Point, either when writing out other compound types, or in a function type signature.  Note that this is just a synonym: as far as the type inferencer is concerned, a Point is exactly the same a record with `x` and `y` fields that are both Ints.  Type synonyms should always begin with an uppercase letter.

### Primitive Types

Eve defines a relatively small set of primitive types, corresponding to the literals in the language.  They are:

* **Bool**.  Represents booleans; legal values are `True` and `False`.
* **Int**.  Represents integers.  Currently, this can be any 32-bit integer; however, it's likely that this will change so that Ints become unbounded, capable of storing any number that fits in the computer's memory.
* **String**.  Represents strings.  

Primitive types should be treated exactly like type synonyms that don't expand to any simpler type, because they *are* the simplest types.

### Tuples

Tuples are compound types that contain zero or more other types:

    ()                  # Empty tuple
    (Int, Int)          # Pairs of integers
    (String, Int, Int)  # A string and two integers

The elements inside a tuple do not all need to all be the same type; this and tuples' fixed length are the main difference between tuples and lists.

Creating a tuple literal looks just like type declarations above, except that you provide expressions instead of type names:

    empty_tuple = ()
    unary_tuple = (1,)
    a_pair = (3, 4)
    sliced_range = ("foobar", 1, 4)

Tuples are frequently used when you want to return multiple values from a function, or pack multiple values into a temporary list.  There are two ways to get values out of a tuple.  The easiest is to pattern-match against it:

    first, second = function_returning_a_tuple()

The other way is to use the .tupleX fields that are automatically defined on tuple types:

    first = (1, 2).tuple1     # first = 1
    second = (1, 2).tuple2    # second = 2

The latter approach works just like [record predicates](#record_predicates), so you can switch from tuples to [records](#records) by ensuring that you have fields or properties named `tuple1`, `tuple2`, etc.  As your data types become more complex, it pays to convert them to records with sensibly named fields, but this equivalency means that you can perform the changeover gradually without breaking existing code.

### Records

Records are like tuples, but instead of denoting components by position, they name them with labeled fields:

    { x: Int, y: Int }
    { val: a, rest: List<a> }
    { val: a, left: Tree<a>, right: Tree<a> }

As with tuples, record literals look like the type declaration, but with expressions instead of type names:

    { x: 2, y: 3 }
    { val: 1, rest: Cons({ val: 2, rest: Nil }) }

You can pattern-match on records:

    { x: x_var, y: y_var } = function_that_returns_a_point()

There's also an abbreviated form of this, commonly called "punning":

    { x, y } = function_that_returns_a_point()

In general, there's a syntactic equivalence between { x, y } and { x: x, y: y }.  This applies for both pattern-matching and record literals.  If you don't provide an expression for a label, the parser will assume that you mean a variable that's identical to that label.

The main way to access fields of a record is through the dot operator:

    x = p.x
    y = p.y

The dot operator is polymorphic, i.e. it doesn't care what the specific type of the record is, only that it has the field that you're asking for.  See the section on [record predicates](#record_predicates) for more.

The syntax for updating a record is:

    { a_point | x: new_x, y: new_y }

Stick the record you're updating before the vertical bar, and then the rest of the expression is like a normal record literal.  Keep in mind that this returns a new record that copies all fields of the original besides the updates: Eve is a functional language, so there is no destructive record update.  Like selection, update is polymorphic; it cares only that the updated fields exist in the record, and not what any of the other fields are.  You cannot add new fields that didn't previously exist.

Records are probably the data type you'll be using most in Eve programming.  In addition to the "naked" records used in these examples, records are frequently used in [union](#unions) and [opaque](#opaque_types) types.

### Functions

As first-class values, functions have their own type.  It's written by putting the argument types in parentheses, and then writing an arrow to the return type:

    (Int, Int) -> Int
    (List<a>, Int) -> a
    (Point, Point) -> Rectangle
    (a -> b, List<a>) -> List<b>
    (List<a>) -> Int where a <: { price: Double, volume: Int }

There's some additional subtlety needed to handle argument defaults, described when we get to interfaces as [variable-length arguments](#variable_length_arguments).  Other than that, first-class function expressions behave exactly like normal function calls.

### Unions

Sometimes, you need a type that can take one of several alternative values.  The `data` statement gives you a means to do this:

    data List<a>: Cons (a, List<a>) | Nil
    data Tree<a>: Branch { left: Tree<a>, val: a, right: Tree<a> } | Leaf
    data Color: Red | Green | Blue
    data Maybe<a>: Just a | Nothing

Each alternative consists of a capitalized tag (called a "constructor") and then some other type.  It's common to use a tuple or record for this, but you can define unions of any type, including type synonyms or variables.  You can also omit the contained type entirely; this is useful for enumerations, where you just want a bunch of symbols to denote unique values.

Unlike with records or tuples, there's no such thing as a "naked" union type, combining values without a constructor tag.  This is because of the type inference algorithm.  If you had a type `data Maybe<a>: a | Nothing`, then when the inferencer encountered any expression, it wouldn't know whether it should be typed as an `a` (whatever type that may be) or as a `Maybe<a>`.  For the same reason, constructors for different data types within the same module cannot overlap.

Each constructor defines a function that takes the enclosed type, and returns a value of the full union type.  For example, to create values of the above types, you'd use:

    a_list = Cons((2, tail))
    a_tree = Branch({ left, right, val: 2 })
    a_color = Red()
    a_list_of_maybes = map(Just, [1, 2, 3])

Pattern matching must occur in the context of a `case` or `recv` statement, because otherwise there's no way to group together the different alternatives or specify an order to try:

    def get(dict, key, default):
        case dict[key]:
            Just val: val
            Nothing: default

Also, each alternative provides a [record predicate](#record_predicates) to extract the original value out of a constructor:

    is_2 = Just(2).Just
    tree_value = node.Branch.val
    list_value = node.Cons.tuple1

If the value doesn't match the specified constructor, or if you try to *update* this field, an exception is raised.

Alternatives that enclose a record type also define record predicates for all of the record's fields.  These work by accessing the necessary constructor accessors, eg. `tree_node.left` = `tree_node.Branch.left`.  This also means that they may raise an exception if the union value is not the proper alternative.  If multiple constructors both have the same record field, the field accessor will work for both of them.

### Opaque types

Remember how type synonyms don't matter to the type inferencer, and all `(Double, Double)` pairs are the same type whether they represent a Point, a Dimension, a PolarCoordinate, or an RangeInterval?  Sometimes you don't want this behavior, and want them to be treated as separate types.  The way to do this is through opaque types:

    data Length: Inches Int
    data PolarCoordinate: Polar (Double, Double)
    data Point: Point (Int, Int)

The astute among you will recognize that this is identical to a union with only one alternative.  In fact, they are exactly the same language construct, and the only difference is what they're used for.  All the operations on union types also apply to opaque types.

If you want additional data hiding (like preventing client code from accessing any enclosed record fields directly), just don't export the record predicates from the module.  This is the default; if you want to expose the internal structure of any record, union, or opaque type, you have to explicitly specify that in the module exports.

## Constraints

Eve does not have subtyping: a variable or other expression can have only one type.  In many cases, it's convenient to be able to define a function that works over a variety of types, as long as they all share certain things in common.  Eve provides the concept of *constraints* for this.

Java programmers should take note: although much of the terminology in this section was stolen from Java and the basic goals are the same, interface constraints are nothing like Java interfaces.  In particular, *they are not types* and do not act like types.  Interface constraints are a separate part of a type declaration, and serve to constrain type variables within the type.  It's much more like the bounded qualification that occurs in Java generics than in the traditional interface mechanism.

Haskell fans may note that constraints are basically the same as typeclasses + associated types + a record system.  This is by design; that system has already proven itself as inferrable and fairly useful, and this is a fairly small facelift over it.

There are three different types of constraints: [interfaces](#interfaces), [record predicates](#record_predicates), and [argument lists](#varargs).  These will each be described in a later section.

### Syntax

Constraints are introduced by the `where` keyword in a type declaration.  For example, the type signature for a function to sort a list might be:

    List<a> -> List<a> where Ord(a)

The Ord(a) constraint reflects the condition that each element be orderable: it must have <, >, and == operations to compare with the other elements.  You might also put constraints on both the container and the element.  For example, if you wanted to generalize this to sorting any *sequence* (where a sequence might also be an array, a stream, a string, etc.), you could have

    container<a> -> container<a> where Sequence(container), Ord(a)

Multiple constraints are separated by commas.

In all constraints, it's important to recognize that type variables always represent the *same* type if they appear more than once.  The example above returns a sequence of the same type that it takes, and the elements of the sequence must all be of the same type.  You can't compare different types that all happen to implement Ord.  (This functionality is desirable, but I can't think of an easy way to implement it at the moment.)

There is a syntactic shorthand for one common case.  If an interface name appears in the type itself, it introduces a *fresh* type variable along with a constraint on that type variable.  For example, this function definition:

    def find(container as Sequence<a>, elem as Eq) -> Int:

results in this type:

    (b<a>, c) -> Int where Sequence(b), Eq(c)

The interface name `Sequence` has been replaced by the type variable `b` and a constraint has been added that specifies that b must implement Sequence.  Similarly, the `Eq` type has been replace by the type `c` and an appropriate constraint added.  Keep in mind that this sort of syntactic expansion generates a *fresh* type variable for each occurence.  For example,

    def sort(container as Sequence) -> Sequence:

would result in a type of `a -> b where Sequence(a), Sequence(b)`, i.e. the argument and result types are not necessarily the *same* sequence type.  This is probably not what you want for this example.

### Interfaces

All the examples so far have been interface constraints.  An interface is basically a contract that a certain set of functions (called "methods") will be available on a type.  You declare an interface with the `interface` keyword:

    interface Eq:
        \== as (self, self) -> Bool
        \!= as (self, self) -> Bool

In the interface declaration, the type variable `self` refers to the type being constrained.  For example, the full type of the == operator is `(a, a) -> Bool where Eq(a)`.

In order to define implementations of an interface (called "instances"), you use the `implements` keyword:

    Point implements Eq:
        def \==(p1, p2): p1.x == p2.x and p1.y == p2.y
        def \!=(p1, p2): p1.x != p2.x or p1.y != p2.y

If you already have suitable implementations available, you can also other binding constructs to define the methods:

    String implements Eq:
        \== = strcmp
        \!= = negate(strcmp)


Interfaces may have *associated types* that define type variables that can be used in the type signatures of the methods.  These are introduced by a `typedef` keyword inside the interface declaration, along with the variable name.  These typedefs have no type attached (unlike typedefs at the top-level), but may have a constraint clause following a `where` keyword.

    interface Iterator:
        typedef elem
        get as self -> elem
        next as self -> self
        is_valid as self -> Bool

    interface PriorityQueue:
        typedef elem where Ord(elem)
        insert as (self, elem) -> self
        find_min as self -> elem
        delete_min as self -> self

Implementations must then specify a concrete type or type variable for each associated type:

    StringIterator implements Iterator
        typedef elem: Char
        ...

    SplayHeap<a> where Ord(a) implements PriorityQueue:
        typedef elem: a
        ...

### Record Predicates

Record predicates are a way to specify that a type has certain fields.  The type declaration

    List<a> where a <: { price: Double, volume: Int }

indicates a list of objects that each have a price and volume field.

The easiest way to generate a record predicate is to have a record.  Records automatically supply record predicates for all their fields, letting you use them in any constrained type that requires that some subset of their fields be present.  Also, `data` type declarations (as used by [unions](#unions) and [opaque types](#opaque_types]) proxy any fields on their alternatives down to the full type.

You can also define "pseudo-fields" for types or type synonyms with the `descriptor` statement:

    descriptor Point:
        def .get_r(p): sqrt(p.x * p.x + p.y * p.y)
        def .set_r(p, r): { p | x: sin(p.theta) * r, y: cos(p.theta) * r }
        def .get_theta(p): atan(p.x / p.y)
        def .set_theta(p, theta): { p | x: sin(theta) * p.r, y: cos(theta) * p.r }

This defines the properties 'r' and 'theta' on any Point objects (i.e. any record of type `{ x: Double, y: Double }`).  You can use these as if they were normal record fields:

    >>> { x: 3.0, y: 4.0 }.r
    5.0
    >>> {{ x: 3.0, y: 4.0 } | r: 10.0 }
    { x: 6.0, y: 8.0 }
    >>> { x: 1.0, y: 1.0 }.theta
    0.78539

This is analogous to the @property decorator in Python or the concept of accessors in CLOS, Dylan, or Ruby.  You access them exactly like normal fields, but can provide computational definitions.

Take note of the names used to define these properties.  They start with `.get_` or `.set_` and then have the field name.  This is the standard way to refer to a field as a first-class function.  For example, you can pull the price field out of each element of a list with (type declaration included for clarity; not needed):

    map(securities as List<a> where a <: { price: Double }, .get_price)

And you could return a new list with a 0.0 price for each element with:

    map(securities, .set_price(?, 0.0))

Record predicates can be combined with ordinary interfaces in the normal way:

    seq<a> where Sequence(seq), a <: { price: Double, volume: Int }

At this time, I haven't decided whether it's possible to have type variables inside a record predicate themselves constrained by other constraints.  I'm inclined to think so, but this may present problems for the typechecker.
