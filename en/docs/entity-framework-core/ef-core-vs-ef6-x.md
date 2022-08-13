---
title: "EF Core vs EF6.x"
slug: "ef-core-vs-ef6x"
draft: false
images: []
weight: 9227
type: docs
toc: true
---

For latest updates, please refer to: [Feature Comparison](https://docs.microsoft.com/en-us/ef/efcore-and-ef6/)


## Side-by-side comparison
The following table compares the features available(1) in EF Core and EF6.x.

It is intended to give a high level comparison and does not list every feature, or attempt to give details on possible differences between how the same feature works.

| Creating a Model | EF6.x | EF Core 1.0.0 |
| ------ | ------ | ----- |
| Basic modelling (classes, properties, etc.)  |  Yes  |  Yes
| Conventions    | Yes    | Yes
| Custom conventions    | Yes    | Partial
| Data annotations    | Yes    | Yes
| Fluent API    | Yes    | Yes
| Inheritance: Table per hierarchy (TPH)    | Yes    | Yes
| Inheritance: Table per type (TPT)    | Yes     
| Inheritance: Table per concrete class (TPC)    | Yes     
| Shadow state properties  |       | Yes
| Alternate keys        | | Yes
| Many-to-many: With join entity    | Yes    | Yes
| Many-to-many: Without join entity    | Yes     
| Key generation: Database    | Yes    | Yes
| Key generation: Client       |  | Yes
| Complex/value types    | Yes     
| Spatial data    | Yes     
| Graphical visualization of model    | Yes     
| Graphical drag/drop editor    | Yes     
| Model format: Code    | Yes    | Yes
| Model format: EDMX (XML)    | Yes     
| Reverse engineer model from database: Command line      |   | Yes
| Reverse engineer model from database: VS wizard    | Yes     
| Incremental update model from database    | Yes     
            
| Querying Data  |  EF6.x   | EF Core 1.0.0
| ------ | ------ | ----- |
| LINQ: Simple queries    | Stable    | Stable
| LINQ: Moderate queries    | Stable   | Stabilizing
| LINQ: Complex queries    | Stable    | In-Progress
| LINQ: Queries using navigation properties    | Stable    | In-Progress
| “Pretty” SQL generation    | Poor    | Yes
| Mixed client/server evaluation       |  | Yes
| Loading related data: Eager    | Yes    | Yes
| Loading related data: Lazy    | Yes     
| Loading related data: Explicit    | Yes     
| Raw SQL queries: Model types    | Yes    | Yes
| Raw SQL queries: Un-mapped types    | Yes     
| Raw SQL queries: Composing with LINQ     |    | Yes
            
| Saving Data   | EF6.x   | EF Core 1.0.0
| ------ | ------ | ----- |
| SaveChanges    | Yes    | Yes
| Change tracking: Snapshot    | Yes    | Yes
| Change tracking: Notification    | Yes    | Yes
| Accessing tracked state    | Yes    | Partial
| Optimistic concurrency    | Yes    | Yes
| Transactions    | Yes    | Yes
| Batching of statements     |    | Yes
| Stored procedure    | Yes     
| Detached graph support (N-Tier): Low level APIs    | Poor    | Yes
| Detached graph support (N-Tier): End-to-end      |   | Poor
            
| Other Features  |  EF6.x |   EF Core 1.0.0
| ------ | ------ | ----- |
| Migrations    | Yes    | Yes
| Database creation/deletion APIs    | Yes    | Yes
| Seed data    | Yes     
| Connection resiliency    | Yes     
| Lifecycle hooks (events, command interception, ...)    | Yes     
            
| Database Providers   | EF6.x  |  EF Core 1.0.0
| ------ | ------ | ----- |
| SQL Server    | Yes    | Yes
| MySQL    | Yes   | Paid only, unpaid coming soon (2)
| PostgreSQL    | Yes    | Yes
| Oracle    | Yes   | Paid only, unpaid coming soon (2)
| SQLite    | Yes    | Yes
| SQL Compact    | Yes    | Yes
| DB2    | Yes    | Yes
| InMemory (for testing)   |      | Yes
| Azure Table Storage       | |  Prototype
| Redis        | | Prototype
            
| Application Models | EF6.x  | EF Core 1.0.0   |
| ------------------ | ------ | --------------- |
| WinForms           | Yes    | Yes             |
| WPF                | Yes    | Yes             |
| Console            | Yes    | Yes             |
| ASP.NET            | Yes    | Yes             |
| ASP.NET Core       |        | Yes             |
| Xamarin            |        | Coming soon (3) |
| UWP                |        | Yes             |

**Footnotes:** 

(1) : As of 2016/10/18 

(2) : Paid providers are available, unpaid providers are being worked on. The teams working on the unpaid providers have not shared public details of timeline etc. 

(3) : EF Core is built to work on Xamarin when support for .NET Standard is enabled in Xamarin.

