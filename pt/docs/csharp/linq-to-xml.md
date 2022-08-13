---
title: "LINQ para XML"
slug: "linq-para-xml"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Leia XML usando LINQ to XML
    <?xml version="1.0" encoding="utf-8" ?>
    <Employees>
     <Employee>
      <EmpId>1</EmpId>
      <Name>Sam</Name>   
      <Sex>Male</Sex>
      <Phone Type="Home">423-555-0124</Phone>
      <Phone Type="Work">424-555-0545</Phone>
      <Address>
       <Street>7A Cox Street</Street>
       <City>Acampo</City>
       <State>CA</State>
       <Zip>95220</Zip>
       <Country>USA</Country>
      </Address>
    </Employee>
    <Employee>
     <EmpId>2</EmpId>
     <Name>Lucy</Name>
     <Sex>Female</Sex>
     <Phone Type="Home">143-555-0763</Phone>
     <Phone Type="Work">434-555-0567</Phone>
     <Address>
      <Street>Jess Bay</Street>
      <City>Alta</City>
      <State>CA</State>
      <Zip>95701</Zip>
      <Country>USA</Country>
      </Address>
     </Employee>
    </Employees>
Para ler esse arquivo XML usando LINQ

    XDocument xdocument = XDocument.Load("Employees.xml");
    IEnumerable<XElement> employees = xdocument.Root.Elements();
    foreach (var employee in employees)
    {
        Console.WriteLine(employee);
    }
Para acessar um único elemento

    XElement xelement = XElement.Load("Employees.xml");
    IEnumerable<XElement> employees = xelement.Root.Elements();
    Console.WriteLine("List of all Employee Names :");
    foreach (var employee in employees)
    {
        Console.WriteLine(employee.Element("Name").Value);
    }

Para acessar vários elementos

    XElement xelement = XElement.Load("Employees.xml");
    IEnumerable<XElement> employees = xelement.Root.Elements();
    Console.WriteLine("List of all Employee Names along with their ID:");
    foreach (var employee in employees)
    {
        Console.WriteLine("{0} has Employee ID {1}",
        employee.Element("Name").Value,
        employee.Element("EmpId").Value);
    }
Para acessar todos os Elementos com um atributo específico

    XElement xelement = XElement.Load("Employees.xml");
    var name = from nm in xelement.Root.Elements("Employee")
           where (string)nm.Element("Sex") == "Female"
           select nm;
    Console.WriteLine("Details of Female Employees:");
    foreach (XElement xEle in name)
    Console.WriteLine(xEle);

Para acessar um elemento específico com um atributo específico

    XElement xelement = XElement.Load("..\\..\\Employees.xml");
    var homePhone = from phoneno in xelement.Root.Elements("Employee")
                where (string)phoneno.Element("Phone").Attribute("Type") == "Home"
                select phoneno;
    Console.WriteLine("List HomePhone Nos.");
    foreach (XElement xEle in homePhone)
    {
        Console.WriteLine(xEle.Element("Phone").Value);
    }

