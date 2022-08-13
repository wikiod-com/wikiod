---
title: "Getting started with json.net"
slug: "getting-started-with-jsonnet"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## How to Install Json.Net in Visual Studio Projects
You can install Json.Net into your Visual Studio Project in 1 of 2 ways.

# Install Json.Net using the Package Manager Console. #

 1. Open the **Package Manager Console** window in Visual Studio either by typing **package manager console** in the Quick Launch box and selecting it

[![enter image description here][2]][2]

or by clicking **View** -> **Other Windows** -> **Package Manager Console**.

2. Once the Package Manager Console is visible, select the project within your solution, into which you want to install Json.Net, by selecting it from the **Default Project** dropdown.

[![enter image description here][3]][3]

3. Now type the following command and press enter.

`Install-Package Newtonsoft.Json`

3. This will install the latest version of Json.Net. You will see the installation progress in the Package Manager Console. If successful, you will the message that it was successfully installed into your selected Project.

[![enter image description here][4]][4]

4. Once installed successfully, you will now be able to see the Json.Net Assembly in the references in your selected Project. The name of the Json.Net assembly is `Newtonsoft.Json`

[![enter image description here][5]][5]

Json.Net is now ready for use in your project!

# Install Json.Net using the Visual Studio Solution Explorer. #

You can also install json.net using the Solution Explorer in Visual Studio. 

1. Right click the **References** node in your Project and click **Manage Nuget Packages...**

2. In the **Nuget Package Manager** Dialog box, make sure **Online** is selected in the left pane. Type **Json.Net** in the search box in the top right. This will display the  Json.Net Nuget Package in the search results pane in the middle. 

3. Click the **Install** button.

[![enter image description here][6]][6]

2. You will see the installation progress in the progress window that will pop up.

[![enter image description here][7]][7]

3. Once the installation is complete, you will see a green check-mark next to the Json.Net package in the **Nuget Package Manager** dialog box.

[![enter image description here][8]][8]

4. You will also see the `Newtonsoft.Json` Assembly in the **References** node in your solution explorer for your selected project.

[![enter image description here][9]][9]

This completes the installation of Json.Net. You are now ready to use it in your project to perform various operations on json data.

  [1]: http://www.newtonsoft.com/json
  [2]: http://i.stack.imgur.com/F5Mz0.png
  [3]: http://i.stack.imgur.com/13Dg6.png
  [4]: http://i.stack.imgur.com/FHwQ5.png
  [5]: http://i.stack.imgur.com/Y6EqM.png
  [6]: http://i.stack.imgur.com/IocDI.png
  [7]: http://i.stack.imgur.com/UZyip.png
  [8]: http://i.stack.imgur.com/2dCla.png
  [9]: http://i.stack.imgur.com/WB3K5.png

## How to Deserialize JSON data to Object using Json.Net in C#
The following example shows how you can deserialize a JSON string containing into an Object (i.e. into an instance of a class).

    using System;
    using System.Collections.Generic;
    using Newtonsoft.Json;
                        
    public class Program
    {
        public class Employee
        {
            public string FirstName { get; set; }
            public string LastName { get; set; }
            public bool IsManager { get; set; }
            public DateTime JoinedDate { get; set; }
            public IList<string> Titles { get; set; }
        }
        
        public static void Main()
        {
            string json = @"{
                              'FirstName': 'Shiva',
                              'LastName': 'Kumar',
                              'IsManager': true,
                              'JoinedDate': '2014-02-10T00:00:00Z',
                              'Titles': [
                                'Sr. Software Engineer',
                                'Applications Architect'
                              ]
                            }";
            
            Employee employee = JsonConvert.DeserializeObject<Employee>(json);

            Console.WriteLine(employee.FirstName);
            Console.WriteLine(employee.LastName);
            Console.WriteLine(employee.JoinedDate);
            foreach (string title in employee.Titles)
            {
                Console.WriteLine("  {0}", title);    
            }
        }
    }

If you run this console program, the output of the various `Console.WriteLine` statements will be as follows.

    Shiva
    Kumar
    2/10/2014 12:00:00 AM
      Sr. Software Engineer
      Applications Architect

**Few things to note**

1. The following line performs the actual deserialization of the data in the json string into the `employee` object instance of the `Employee` class.

`Employee employee = JsonConvert.DeserializeObject<Employee>(json);`

2. Since `employee.Titles` is a `List<string>` type, we use the `foreach` loop construct to loop through each item in that `List`.

## How to Serialize an Object to JSON using Json.Net in C#
The following example shows how you can use Json.Net to serialize the data in an C# Object's instance, to JSON string.

    using System;
    using System.Collections.Generic;
    using Newtonsoft.Json;
    
    public class Employee
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }
        public bool IsManager { get; set; }
        public DateTime JoinedDate { get; set; }
        public IList<string> Titles { get; set; }
    }
                        
    public class Program
    {
        public static void Main()
        {
            Employee employee = new Employee
            {
                FirstName = "Shiva",
                LastName = "Kumar",            
                IsManager = true,
                JoinedDate = new DateTime(2013, 1, 20, 0, 0, 0, DateTimeKind.Utc),
                Titles = new List<string>
                {
                    "Sr. Software Engineer",
                    "Applications Architect"
                }
            };
    
            string json = JsonConvert.SerializeObject(employee, Formatting.Indented);
    
    
    Console.WriteLine(json);
            
        }
    }

If you run this console program, the output of the `Console.WriteLine(json)` will be

    {
      "FirstName": "Shiva",
      "LastName": "Kumar",
      "IsManager": true,
      "JoinedDate": "2013-01-20T00:00:00Z",
      "Titles": [
        "Sr. Software Engineer",
        "Applications Architect"
      ]
    }

**Few things to note**
1. The following line performs the actual serialization of the data inside the `employee` class instance into a json string

    `string json = JsonConvert.SerializeObject(employee, Formatting.Indented);`

2. The parameter `Formatting.Indented` tells Json.Net to serialize the data with indentation and new lines. If you don't do that, the serialized string will be one long string with no indentation or line breaks.



