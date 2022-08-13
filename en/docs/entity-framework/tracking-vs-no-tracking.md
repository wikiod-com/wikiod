---
title: "Tracking vs. No-Tracking"
slug: "tracking-vs-no-tracking"
draft: false
images: []
weight: 9321
type: docs
toc: true
---

Tracking behavior controls whether or not Entity Framework will keep information about an entity instance in its change tracker. If an entity is tracked, any changes detected in the entity will be persisted to the database during `SaveChanges()`. 

## No-tracking queries
 - No tracking queries are useful when the results are used in a
   `read-only` scenario
 - They are `quicker to execute` because there is no need to setup change
   tracking information

**Example :**

    using (var context = new BookContext())
    {
        var books = context.Books.AsNoTracking().ToList();
    }

With EF Core 1.0 you are also able to change the default tracking behavior at the `context
   instance` level.

**Example :**

    using (var context = new BookContext())
    {
        context.ChangeTracker.QueryTrackingBehavior = QueryTrackingBehavior.NoTracking;
    
        var books = context.Books.ToList();
    }



## Tracking queries
 - By default, queries that return entity types are **tracking**
 - This means you can make changes to those entity instances and have
   those changes persisted by `SaveChanges()`

**Example :** 

 - The change to the `book` rating will be detected and persisted to the
   database during `SaveChanges()`.

 

      using (var context = new BookContext())
      {
        var book = context.Books.FirstOrDefault(b => b.BookId == 1);
        book.Rating = 5;
        context.SaveChanges();
      }

## Tracking and projections
 - Even if the result type of the query isn’t an entity type, if the
   result `contains entity` types they will still be `tracked by default`

**Example :** 

 - In the following query, which returns an `anonymous type`, the
   instances of `Book` in the result set `will be tracked`

        using (var context = new BookContext())
        {
           var book = context.Books.Select(b => new { Book = b, Authors = b.Authors.Count() });
        }

 - If the result set `does not` contain any `entity` types, then `no tracking`
   is performed

**Example :**

 - In the following query, which returns an `anonymous type` with some of
   the values from the entity (but `no instances` of the actual `entity`
   type), there is **no tracking** performed.

       using (var context = new BookContext())
       {
          var book = context.Books.Select(b => new { Id = b.BookId, PublishedDate = b.Date });
       }

