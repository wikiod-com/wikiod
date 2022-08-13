---
title: "Standard Query Operators"
slug: "standard-query-operators"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

Linq queries are written using the [Standard Query Operators][1] (which are a set of extension methods that operates mainly on objects of type `IEnumerable<T>` and `IQueryable<T>`) or using [Query Expressions][2] (which at compile time, are converted to Standard Query Operator method calls).

Query operators provide query capabilities including filtering, projection, aggregation, sorting and more.


  [1]: https://msdn.microsoft.com/en-us/library/mt693029.aspx
  [2]: https://msdn.microsoft.com/en-us/library/bb397676(v=vs.100).aspx

## Aggregation Operations
Aggregation operations computes a single value from a collection of values.

**Aggregate**

> Performs a custom aggregation operation on the values of a collection.

Method Syntax

    // Aggregate

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var product = numbers.Aggregate(1, (acc, n) => acc * n);

    // product = 120

Query Syntax

    // Not applicable.

----

**Average**

> Calculates the average value of a collection of values.

Method Syntax

    // Average

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var average = numbers.Average();

    // average = 3

Query Syntax

    // Not applicable.

----

**Count**

> Counts the elements in a collection, optionally only those elements that satisfy a predicate function.

Method Syntax

    // Count

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    int count = numbers.Count(n => n % 2 == 0);

    // count = 2

Query Syntax

    // Not applicable.

----

**LongCount**

> Counts the elements in a large collection, optionally only those elements that satisfy a predicate function.

Method Syntax

    // LongCount

    var numbers = new int[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

    long count = numbers.LongCount();

    // count = 10

Query Syntax

    // Not applicable.

----

**Max**

> Determines the maximum value in a collection. Throws exception if collection is empty.

Method Syntax

    // Max

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var max = numbers.Max();

    // max = 5

Query Syntax

    // Not applicable.

----

**Min**

> Determines the minimum value in a collection. Throws exception if collection is empty.

Method Syntax

    // Min

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var min = numbers.Min();

    // min = 1

Query Syntax

    // Not applicable.

----
**Min-/MaxOrDefault**

> Unlike other LinQ extensions `Min()` and `Max()` do not have an overload without exceptions. Therefor the `IEnumerable` must be checked for `Any()` before calling `Min()` or `Max()`

    // Max
    
    var numbers = new int[] { };

    var max = numbers.Any() ? numbers.Max() : 0;

    // max = 0

----

**Sum**

> Calculates the sum of the values in a collection.

Method Syntax

    // Sum

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var sum = numbers.Sum();

    // sum = 15

Query Syntax

    // Not applicable.

## Partition Operations
Partitioning refers to the operations of dividing an input sequence into two sections, without rearranging the elements, and then returning one of the sections.

**Skip**

> Skips elements up to a specified position in a sequence.

Method Syntax

    // Skip

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var skipped = numbers.Skip(3);

    // skipped = { 4, 5 }

Query Syntax

    // Not applicable.

----

**SkipWhile**

> Skips elements based on a predicate function until an element does not satisfy the condition.

Method Syntax

    // Skip

    var numbers = new int[] { 1, 3, 5, 2, 1, 3, 5 };

    var skipLeadingOdds = numbers.SkipWhile(n => n % 2 != 0);

    // skipLeadingOdds = { 2, 1, 3, 5 }

Query Syntax

    // Not applicable.

----

**Take**

> Takes elements up to a specified position in a sequence.

Method Syntax

    // Take

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var taken = numbers.Take(3);

    // taken = { 1, 2, 3 }

Query Syntax

    // Not applicable.

----

**TakeWhile**

> Takes elements based on a predicate function until an element does not satisfy the condition.

Method Syntax

    // TakeWhile

    var numbers = new int[] { 1, 3, 5, 2, 1, 3, 5 };

    var takeLeadingOdds = numbers.TakeWhile(n => n % 2 != 0);

    // takeLeadingOdds = { 1, 3, 5 }

Query Syntax

    // Not applicable.

## Concatenation Operations
Concatenation refers to the operation of appending one sequence to another.

**Concat**

> Concatenates two sequences to form one sequence.

Method Syntax

    // Concat

    var numbers1 = new int[] { 1, 2, 3 };
    var numbers2 = new int[] { 4, 5, 6 };

    var numbers = numbers1.Concat(numbers2);

    // numbers = { 1, 2, 3, 4, 5, 6 }

Query Syntax

    // Not applicable.

## Filtering Operations
Filtering refers to the operations of restricting the result set to contain only those elements that satisfy a specified condition.

**Where**

> Selects values that are based on a predicate function.

Method Syntax

    // Where

    var numbers = new int[] { 1, 2, 3, 4, 5, 6, 7, 8 };

    var evens = numbers.Where(n => n % 2 == 0);

    // evens = { 2, 4, 6, 8 }

Query Syntax

    // where

    var numbers = new int[] { 1, 2, 3, 4, 5, 6, 7, 8 };

    var odds = from n in numbers
               where n % 2 != 0
               select n;

    // odds = { 1, 3, 5, 7 }

----

**OfType**

> Selects values, depending on their ability to be cast to a specified type.

Method Syntax

    // OfType

    var numbers = new object[] { 1, "one", 2, "two", 3, "three" };

    var strings = numbers.OfType<string>();

    // strings = { "one", "two", "three" }

Query Syntax

    // Not applicable.

## Projection Operations
Projection refers to the operations of transforming an object into a new form.

**Select**

> Projects values that are based on a transform function.

Method Syntax

    // Select

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var strings = numbers.Select(n => n.ToString());

    // strings = { "1", "2", "3", "4", "5" }

Query Syntax

    // select

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var strings = from n in numbers
                  select n.ToString();

    // strings = { "1", "2", "3", "4", "5" }

----

**SelectMany**

> Projects sequences of values that are based on a transform function and then flattens them into one sequence.

Method Syntax

    // SelectMany

    class Customer
    {
        public Order[] Orders { get; set; }
    }

    class Order
    {
        public Order(string desc) { Description = desc; }
        public string Description { get; set; }
    }
    ...

    var customers = new Customer[] 
    {
        new Customer { Orders = new Order[] { new Order("O1"), new Order("O2") } },
        new Customer { Orders = new Order[] { new Order("O3") } },
        new Customer { Orders = new Order[] { new Order("O4") } },
    };

    var orders = customers.SelectMany(c => c.Orders);

    // orders = { Order("O1"), Order("O3"), Order("O3"), Order("O4") }

Query Syntax

    // multiples from

    var orders = from c in customers
                 from o in c.Orders
                 select o;

    // orders = { Order("O1"), Order("O3"), Order("O3"), Order("O4") }

## Sorting Operations
A sorting operation orders the elements of a sequence based on one or more attributes.

**OrderBy**

> Sorts values in ascending order.

Method Syntax

    // OrderBy

    var numbers = new int[] { 5, 4, 8, 2, 7, 1, 9, 3, 6 };

    var ordered = numbers.OrderBy(n => n);

    // ordered = { 1, 2, 3, 4, 5, 6, 7, 8, 9 }

Query Syntax

    // orderby

    var numbers = new int[] { 5, 4, 8, 2, 7, 1, 9, 3, 6 };

    var ordered = from n in numbers
                  orderby n
                  select n;

    // ordered = { 1, 2, 3, 4, 5, 6, 7, 8, 9 }

----

**OrderByDescending**

> Sorts values in descending order.

Method Syntax

    // OrderByDescending

    var numbers = new int[] { 5, 4, 8, 2, 7, 1, 9, 3, 6 };

    var ordered = numbers.OrderByDescending(n => n);

    // ordered = { 9, 8, 7, 6, 5, 4, 3, 2, 1 }

Query Syntax

    // orderby

    var numbers = new int[] { 5, 4, 8, 2, 7, 1, 9, 3, 6 };

    var ordered = from n in numbers
                  orderby n descending
                  select n;

    // ordered = { 9, 8, 7, 6, 5, 4, 3, 2, 1 }

----

**ThenBy**

> Performs a secondary sort in ascending order.

Method Syntax

    // ThenBy

    string[] words = { "the", "quick", "brown", "fox", "jumps" };

    var ordered = words.OrderBy(w => w.Length).ThenBy(w => w[0]);

    // ordered = { "fox", "the", "brown", "jumps", "quick" }

Query Syntax

    // orderby …, …

    string[] words = { "the", "quick", "brown", "fox", "jumps" };

    var ordered = from w in words
                  orderby w.Length, w[0]
                  select w;

    // ordered = { "fox", "the", "brown", "jumps", "quick" }

----

**ThenByDescending**

> Performs a secondary sort in descending order.

Method Syntax

    // ThenByDescending

    string[] words = { "the", "quick", "brown", "fox", "jumps" };

    var ordered = words.OrderBy(w => w[0]).ThenByDescending(w => w.Length);

    // ordered = { "brown", "fox", "jumps", "quick", "the" }

Query Syntax

    // orderby …, … descending

    string[] words = { "the", "quick", "brown", "fox", "jumps" };

    var ordered = from w in words
                  orderby w.Length, w[0] descending
                  select w;

    // ordered = { "the", "fox", "quick", "jumps", "brown" }

----

**Reverse**

> Reverses the order of the elements in a collection.

Method Syntax

    // Reverse

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var reversed = numbers.Reverse();

    // reversed = { 5, 4, 3, 2, 1 }

Query Syntax

    // Not applicable.

## Generation Operations
Generation refers to creating a new sequence of values.

**DefaultIfEmpty**

> Replaces an empty collection with a default valued singleton collection.

Method Syntax

    // DefaultIfEmpty

    var nums = new int[0];

    var numbers = nums.DefaultIfEmpty();

    // numbers = { 0 }

Query Syntax

    // Not applicable.

----

**Empty**

> Returns an empty collection.

Method Syntax

    // Empty

    var empty = Enumerable.Empty<string>();

    // empty = IEnumerable<string> { }

Query Syntax

    // Not applicable.

----

**Range**

> Generates a collection that contains a sequence of numbers.

Method Syntax

    // Range

    var range = Enumerable.Range(1, 5);

    // range = { 1, 2, 3, 4, 5 }

Query Syntax

    // Not applicable.

----

**Repeat**

> Generates a collection that contains one repeated value.

Method Syntax

    // Repeat

    var repeats = Enumerable.Repeat("s", 3);

    // repeats = { "s", "s", "s" }

Query Syntax

    // Not applicable.

## Set Operations
Set operations refer to query operations that produce a result set that is based on the presence or absence of equivalent elements within the same or separate collections (or sets).

**Distinct**

> Removes duplicate values from a collection.

Method Syntax

    // Distinct

    var numbers = new int[] { 1, 2, 3, 1, 2, 3 };

    var distinct = numbers.Distinct();

    // distinct = { 1, 2, 3 }

Query Syntax

    // Not applicable.

----

**Except**

> Returns the set difference, which means the elements of one collection that do not appear in a second collection.

Method Syntax

    // Except

    var numbers1 = new int[] { 1, 2, 3, 4, 5 };
    var numbers2 = new int[] { 4, 5, 6, 7, 8 };

    var except = numbers1.Except(numbers2);

    // except = { 1, 2, 3 }

Query Syntax

    // Not applicable.

----

**Intersect**

> Returns the set intersection, which means elements that appear in each of two collections.

Method Syntax

    // Intersect

    var numbers1 = new int[] { 1, 2, 3, 4, 5 };
    var numbers2 = new int[] { 4, 5, 6, 7, 8 };

    var intersect = numbers1.Intersect(numbers2);

    // intersect = { 4, 5 }

Query Syntax

    // Not applicable.

----

**Union**

> Returns the set union, which means unique elements that appear in either of two collections.

Method Syntax

    // Union

    var numbers1 = new int[] { 1, 2, 3, 4, 5 };
    var numbers2 = new int[] { 4, 5, 6, 7, 8 };

    var union = numbers1.Union(numbers2);

    // union = { 1, 2, 3, 4, 5, 6, 7, 8 }

Query Syntax

    // Not applicable.

## Equality Operations
Two sequences whose corresponding elements are equal and which have the same number of elements are considered equal.

**SequenceEqual**

> Determines whether two sequences are equal by comparing elements in a pair-wise manner.

Method Syntax

    // SequenceEqual

    var numbers1 = new int[] { 1, 2, 3, 4, 5 };
    var numbers2 = new int[] { 1, 2, 3, 4, 5 };

    var equals = numbers1.SequenceEqual(numbers2);

    // equals = true

Query Syntax

    // Not Applicable.

## Join Operations
A join of two data sources is the association of objects in one data source with objects that share a common attribute in another data source.

**Join**

> Joins two sequences based on key selector functions and extracts pairs of values.

Method Syntax

    // Join

    class Customer
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }

    class Order
    {
        public string Description { get; set; }
        public int CustomerId { get; set; }
    }
    ...

    var customers = new Customer[] 
    {
        new Customer { Id = 1, Name = "C1" },
        new Customer { Id = 2, Name = "C2" },
        new Customer { Id = 3, Name = "C3" }
    };

    var orders = new Order[]
    {
        new Order { Description = "O1", CustomerId = 1 },
        new Order { Description = "O2", CustomerId = 1 },
        new Order { Description = "O3", CustomerId = 2 },
        new Order { Description = "O4", CustomerId = 3 },
    };

    var join = customers.Join(orders, c => c.Id, o => o.CustomerId, (c, o) => c.Name + "-" + o.Description);

    // join = { "C1-O1", "C1-O2", "C2-O3", "C3-O4" }

Query Syntax

    // join … in … on … equals …

    var join = from c in customers
               join o in orders
               on c.Id equals o.CustomerId
               select o.Description + "-" + c.Name;

    // join = { "O1-C1", "O2-C1", "O3-C2", "O4-C3" }

----

**GroupJoin**

> Joins two sequences based on key selector functions and groups the resulting matches for each element.

Method Syntax

    // GroupJoin

    var groupJoin = customers.GroupJoin(orders,
                                        c => c.Id, 
                                        o => o.CustomerId, 
                                        (c, ors) => c.Name + "-" + string.Join(",", ors.Select(o => o.Description)));

    // groupJoin = { "C1-O1,O2", "C2-O3", "C3-O4" }

Query Syntax

    // join … in … on … equals … into …

    var groupJoin = from c in customers
                    join o in orders               
                    on c.Id equals o.CustomerId
                    into customerOrders
                    select string.Join(",", customerOrders.Select(o => o.Description)) + "-" + c.Name;

    // groupJoin = { "O1,O2-C1", "O3-C2", "O4-C3" }
    
**Zip**

> Applies a specified function to the corresponding elements of two sequences, producing a sequence of the results.

    var numbers = new [] { 1, 2, 3, 4, 5, 6 };
    var words = new [] { "one", "two", "three" };
    
    var numbersWithWords =
        numbers
        .Zip(
            words,
            (number, word) => new { number, word });
    
    // Results

    //| number | word   |
    //| ------ | ------ |
    //| 1      |  one   |
    //| 2      |  two   |
    //| 3      |  three |


## Conversion Operations
Conversion operations change the type of input objects.

**AsEnumerable**

> Returns the input typed as IEnumerable<T>.

Method Syntax

    // AsEnumerable

    int[] numbers = { 1, 2, 3, 4, 5 };

    var nums = numbers.AsEnumerable();

    // nums: static type is IEnumerable<int>

Query Syntax

    // Not applicable.

----

**AsQueryable**

> Converts a IEnumerable<T> to a IQueryable<T>.

Method Syntax

    // AsQueryable

    int[] numbers = { 1, 2, 3, 4, 5 };

    var nums = numbers.AsQueryable();

    // nums: static type is IQueryable<int>

Query Syntax

    // Not applicable.

----

**Cast**

> Casts the elements of a collection to a specified type.

Method Syntax

    // Cast

    var numbers = new object[] { 1, 2, 3, 4, 5 };

    var nums = numbers.Cast<int>();

    // nums: static type is IEnumerable<int>

Query Syntax

    // Use an explicitly typed range variable.

    var numbers = new object[] { 1, 2, 3, 4, 5 };

    var nums = from int n in numbers select n;

    // nums: static type is IEnumerable<int>

----

**OfType**

> Filters values, depending on their ability to be cast to a specified type.

Method Syntax

    // OfType

    var objects = new object[] { 1, "one", 2, "two", 3, "three" };

    var numbers = objects.OfType<int>();

    // nums = { 1, 2, 3 }

Query Syntax

    // Not applicable.

----

**ToArray**

> Converts a collection to an array.

Method Syntax

    // ToArray

    var numbers = Enumerable.Range(1, 5);

    int[] array = numbers.ToArray();

    // array = { 1, 2, 3, 4, 5 }

Query Syntax

    // Not applicable.

----

**ToList**

> Converts a collection to a list.

Method Syntax

    // ToList

    var numbers = Enumerable.Range(1, 5);

    List<int> list = numbers.ToList();

    // list = { 1, 2, 3, 4, 5 }

Query Syntax

    // Not applicable.

----

**ToDictionary**

> Puts elements into a dictionary based on a key selector function.

Method Syntax

    // ToDictionary

    var numbers = new int[] { 1, 2, 3 };

    var dict = numbers.ToDictionary(n => n.ToString());

    // dict = { "1" => 1, "2" => 2, "3" => 3 }

Query Syntax

    // Not applicable.

## Quantifier Operations
Quantifier operations return a Boolean value that indicates whether some or all of the elements in a sequence satisfy a condition.

**All**

> Determines whether all the elements in a sequence satisfy a condition.

Method Syntax

    // All

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    bool areLessThan10 = numbers.All(n => n < 10);

    // areLessThan10 = true

Query Syntax

    // Not applicable.

----

**Any**

> Determines whether any elements in a sequence satisfy a condition.

Method Syntax

    // Any

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    bool anyOneIsEven = numbers.Any(n => n % 2 == 0);

    // anyOneIsEven = true

Query Syntax

    // Not applicable.

----

**Contains**

> Determines whether a sequence contains a specified element.

Method Syntax

    // Contains

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    bool appears = numbers.Contains(10);

    // appears = false

Query Syntax

    // Not applicable.

## Grouping Operations
Grouping refers to the operations of putting data into groups so that the elements in each group share a common attribute.

**GroupBy**

> Groups elements that share a common attribute.

Method Syntax

    // GroupBy

    class Order
    {
        public string Customer { get; set; }
        public string Description { get; set; }
    }
    ...

    var orders = new Order[] 
    {
        new Order { Customer = "C1", Description = "O1" },
        new Order { Customer = "C2", Description = "O2" },
        new Order { Customer = "C3", Description = "O3" },
        new Order { Customer = "C1", Description = "O4" },
        new Order { Customer = "C1", Description = "O5" },
        new Order { Customer = "C3", Description = "O6" },
    };

    var groups = orders.GroupBy(o => o.Customer);

    // groups: { (Key="C1", Values="O1","O4","O5"), (Key="C2", Values="O2"), (Key="C3", Values="O3","O6") }

Query Syntax

    // group … by

    var groups = from o in orders
                 group o by o.Customer;

    // groups: { (Key="C1", Values="O1","O4","O5"), (Key="C2", Values="O2"), (Key="C3", Values="O3","O6") }

----

**ToLookup**

> Inserts elements into a one-to-many dictionary based on a key selector function.

Method Syntax

    // ToLookUp

    var ordersByCustomer = orders.ToLookup(o => o.Customer);

    // ordersByCustomer = ILookUp<string, Order>
    // {
    //     "C1" => { Order("01"), Order("04"), Order("05") },
    //     "C2" => { Order("02") },
    //     "C3" => { Order("03"), Order("06") }
    // }

Query Syntax

    // Not applicable.

## Element Operations
Element operations return a single, specific element from a sequence.

**ElementAt**

> Returns the element at a specified index in a collection.

Method Syntax

    // ElementAt

    var strings = new string[] { "zero", "one", "two", "three" };

    var str = strings.ElementAt(2);

    // str = "two"

Query Syntax

    // Not Applicable.

----

**ElementAtOrDefault**

> Returns the element at a specified index in a collection or a default value if the index is out of range.

Method Syntax

    // ElementAtOrDefault

    var strings = new string[] { "zero", "one", "two", "three" };

    var str = strings.ElementAtOrDefault(10);

    // str = null

Query Syntax

    // Not Applicable.

----

**First**

> Returns the first element of a collection, or the first element that satisfies a condition.

Method Syntax

    // First

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var first = strings.First();

    // first = 1

Query Syntax

    // Not Applicable.

----

**FirstOrDefault**

> Returns the first element of a collection, or the first element that satisfies a condition. Returns a default value if no such element exists.

Method Syntax

    // FirstOrDefault

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var firstGreaterThanTen = strings.FirstOrDefault(n => n > 10);

    // firstGreaterThanTen = 0

Query Syntax

    // Not Applicable.

----

**Last**

> Returns the last element of a collection, or the last element that satisfies a condition.

Method Syntax

    // Last

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var last = strings.Last();

    // last = 5

Query Syntax

    // Not Applicable.

----

**LastOrDefault**

> Returns the last element of a collection, or the last element that satisfies a condition. Returns a default value if no such element exists.

Method Syntax

    // LastOrDefault

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var lastGreaterThanTen = strings.LastOrDefault(n => n > 10);

    // lastGreaterThanTen = 0

Query Syntax

    // Not Applicable.

----

**Single**

> Returns the only element of a collection, or the only element that satisfies a condition.

Method Syntax

    // Single

    var numbers = new int[] { 1 };

    var single = strings.Single();

    // single = 1

Query Syntax

    // Not Applicable.

----

**SingleOrDefault**

> Returns the only element of a collection, or the only element that satisfies a condition. Returns a default value if no such element exists or the collection does not contain exactly one element.

Method Syntax

    // SingleOrDefault

    var numbers = new int[] { 1, 2, 3, 4, 5 };

    var singleGreaterThanFour = strings.SingleOrDefault(n => n > 4);

    // singleGreaterThanFour = 5

Query Syntax

    // Not Applicable.

