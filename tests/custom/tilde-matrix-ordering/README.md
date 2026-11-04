# Predefined matrix ordering

Each Essence instance has a saved set of JSON solution records in
`*.solutions.expected`. The custom harness compares every solution, including
duplicates, after sorting complete records to ignore solver enumeration order.
`--validate-solutions` also checks each answer against its Essence instance.

The expected answer sets were independently enumerated during development.
There is no Python dependency or bespoke solution parser in this test.

These fixtures use JSON solutions because the plain solution printer loses
index tags on transformed matrix decisions. The exhaustive harness's subsequent
validation then rejects their index domains. JSON solution validation handles
these cases. The custom harness is also used by the existing symmetry tests.
