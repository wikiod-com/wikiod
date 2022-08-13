---
title: "Design pattern implementation in F#"
slug: "design-pattern-implementation-in-f"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Data-driven programming in F#
Thanks to type-inference and partial application in `F#` [data-driven programming](http://stackoverflow.com/questions/1065584/what-is-data-driven-programming) is succinct and readable.

Let's imagine we are selling car insurance. Before we try selling it to a customer,
we try to determine if the customer is a valid potential customer for our company by checking the customer's sex and age.

A simple customer model:

    type Sex =
      | Male
      | Female

    type Customer =
      {
        Name    : string
        Born    : System.DateTime
        Sex     : Sex
      }

Next we want to define an exclusion list (table) so that if a customer matches any row
in the exclusion list the customer is cannot buy our car insurance.

    //  If any row in this list matches the Customer, the customer isn't eligible for the car insurance.
    let exclusionList =
      let __          _   = true
      let olderThan   x y = x < y
      let youngerThan x y = x > y
      [|
    //  Description                         Age               Sex
        "Not allowed for senior citizens" , olderThan   65  , __
        "Not allowed for children"        , youngerThan 16  , __
        "Not allowed for young males"     , youngerThan 25  , (=) Male
      |]

Because of type-inference and partial application, the exclusion list is flexible yet
easy to understand.

Finally, we define a function that uses the exclusion list (a table) to split the customers into two buckets: potential and denied customers.

    // Splits customers into two buckets: potential customers and denied customers.
    // The denied customer bucket also includes the reason for denying them the car insurance
    let splitCustomers (today : System.DateTime) (cs : Customer []) : Customer []*(string*Customer) [] =
      let potential = ResizeArray<_> 16 // ResizeArray is an alias for System.Collections.Generic.List
      let denied    = ResizeArray<_> 16

      for c in cs do
        let age = today.Year - c.Born.Year
        let sex = c.Sex
        match exclusionList |> Array.tryFind (fun (_, testAge, testSex) -> testAge age && testSex sex) with
        | Some (description, _, _)  -> denied.Add (description, c)
        | None                      -> potential.Add c

      potential.ToArray (), denied.ToArray ()

To wrap up, let's define some customers and see if they are any potential customers
for our car insurance amongst them:

    let customers =
      let c n s y m d: Customer = { Name = n; Born = System.DateTime (y, m, d); Sex = s }
      [|
    //    Name                      Sex     Born
        c "Clint Eastwood Jr."      Male    1930 05 31
        c "Bill Gates"              Male    1955 10 28
        c "Melina Gates"            Female  1964 08 15
        c "Justin Drew Bieber"      Male    1994 03 01
        c "Sophie Turner"           Female  1996 02 21
        c "Isaac Hempstead Wright"  Male    1999 04 09
      |]

    [<EntryPoint>]
    let main argv =
      let potential, denied = splitCustomers (System.DateTime (2014, 06, 01)) customers
      printfn "Potential Customers (%d)\n%A" potential.Length potential
      printfn "Denied Customers (%d)\n%A"    denied.Length    denied
      0

This prints:

    Potential Customers (3)
    [|{Name = "Bill Gates";
       Born = 1955-10-28 00:00:00;
       Sex = Male;}; {Name = "Melina Gates";
                      Born = 1964-08-15 00:00:00;
                      Sex = Female;}; {Name = "Sophie Turner";
                                       Born = 1996-02-21 00:00:00;
                                       Sex = Female;}|]
    Denied Customers (3)
    [|("Not allowed for senior citizens", {Name = "Clint Eastwood Jr.";
                                           Born = 1930-05-31 00:00:00;
                                           Sex = Male;});
      ("Not allowed for young males", {Name = "Justin Drew Bieber";
                                       Born = 1994-03-01 00:00:00;
                                       Sex = Male;});
      ("Not allowed for children", {Name = "Isaac Hempstead Wright";
                                    Born = 1999-04-09 00:00:00;
                                    Sex = Male;})|]


