# Flavors

Due to a bias for Flavors dating back to the early days of LISP on
Symbolics machines Flavors was implemented in SLIP first. Flavors has
the advantage over generics in that methods are encapsulated in the
Flavor itself with a localized name space for method names. This makes
it easier to develop components or Flavors separately without concern
of conflicts in method namespace.

The SLIP Flavors implementation follows the Flavors API from the
Symbolics days. A Flavor has fields to store the definition of Flavor
and instances of the Flavor have a reference to the Flavor. Since
methods are shared across all instances of a Flavor the methods are
stored on the Flavor. Variable definitions are stored on the Flavor
while variable values are stored in a map on then instances.

One of then defining features of Flavors are the method daemons. In
CLOS method daemons are referred to as qualifiers. Only the default
method combination is implement with before, after, and whoppers
(around) daemons. A method struct includes lists of each daemon as
well as the primary. When a method is called the daemons are called in
the appropriate order. Methods are built when Flavors are inherited
which places method daemons from inherited Flavors on then method
daemon lists.
