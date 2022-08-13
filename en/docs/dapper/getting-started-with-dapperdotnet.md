---
title: "Getting started with Dapper.NET"
slug: "getting-started-with-dappernet"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Install Dapper from Nuget
Either search in the Visual Studio GUI:

Tools > NuGet Package Manager > Manage Packages for Solution... (Visual Studio 2015)

[![screenshot of the Visual Studio package manager interface with Dapper being selected][1]][1]

Or run this command in a Nuget Power Shell instance to install the latest stable version

    Install-Package Dapper

Or for a specific version

    Install-Package Dapper -Version 1.42.0

  [1]: http://i.stack.imgur.com/sWn6V.png

## Using Dapper in C#
    using System.Data;
    using System.Linq;
    using Dapper;
    
    class Program
    {
        static void Main()
        {
            using (IDbConnection db = new SqlConnection("Server=myServer;Trusted_Connection=true"))
            {
                db.Open();
                var result = db.Query<string>("SELECT 'Hello World'").Single();
                Console.WriteLine(result);
            }
        }
    }

Wrapping the connection in a [`Using` block](https://www.wikiod.com/docs/c%23/38/using-statement/157/cleaner-dispose-syntax) will close the connection

## Using Dapper in LINQPad
[LINQPad](http://www.linqpad.net/) is great for testing database queries and includes [NuGet integration](http://www.linqpad.net/Purchase.aspx#NuGet). To use Dapper in LINQPad press **F4** to open the Query Properties and then select **Add NuGet**. Search for **dapper dot net** and select **Add To Query**. You will also want to click **Add namespaces** and highlight Dapper to include the Extension Methods in your LINQPad query.

Once Dapper is enabled you can change the Language drop down to **C# Program**, map query results to C# classes, and use the .Dump() method to inspect the results:

	void Main()
	{
		using (IDbConnection db = new SqlConnection("Server=myServer;Trusted_Connection=true")){
			db.Open();
			var scalar = db.Query<string>("SELECT GETDATE()").SingleOrDefault();
			scalar.Dump("This is a string scalar result:");
			
			var results = db.Query<myobject>(@"
			SELECT * FROM (
			VALUES (1,'one'),
				(2,'two'),
				(3,'three')
			) AS mytable(id,name)");
			results.Dump("This is a table mapped to a class:");
		}
	}
	
	// Define other methods and classes here
	class myobject {
		public int id { get; set; }
		public string name { get; set; }
	}

The results when executing the program would look like this:

[![LINQPad screenshot][1]][1]


  [1]: http://i.stack.imgur.com/swXB1.png

