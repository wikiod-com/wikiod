---
title: "Working with XML Files"
slug: "working-with-xml-files"
draft: false
images: []
weight: 9799
type: docs
toc: true
---

## Accessing an XML File
    <!-- file.xml -->
    <people>
        <person id="101">
            <name>Jon Lajoie</name>
            <age>22</age>
        </person>
        <person id="102">
            <name>Lord Gaben</name>
            <age>65</age>
        </person>
        <person id="103">
            <name>Gordon Freeman</name>
            <age>29</age>
        </person>
    </people>
----------
***Loading an XML File***

To load an XML file, you can use any of these:

    # First Method
    $xdoc = New-Object System.Xml.XmlDocument
    $file = Resolve-Path(".\file.xml")
    $xdoc.load($file)

    # Second Method
    [xml] $xdoc = Get-Content ".\file.xml"
    
    # Third Method
    $xdoc = [xml] (Get-Content ".\file.xml")

----------
***Accessing XML as Objects***

    PS C:\> $xml = [xml](Get-Content file.xml)
    PS C:\> $xml
    
    PS C:\> $xml.people
    
    person
    --------
    {Jon Lajoie, Lord Gaben, Gordon Freeman}
    
    PS C:\> $xml.people.person
    
    id                                      name                                    age
    --                                      ----                                    ---
    101                                     Jon Lajoie                              22
    102                                     Lord Gaben                              65
    103                                     Gordon Freeman                          29
    
    PS C:\> $xml.people.person[0].name
    Jon Lajoie
    
    PS C:\> $xml.people.person[1].age
    65

    PS C:\> $xml.people.person[2].id
    103

----------


***Accessing XML with XPath***

    PS C:\> $xml = [xml](Get-Content file.xml)
    PS C:\> $xml
    
    PS C:\> $xml.SelectNodes("//people")
    
    person
    --------
    {Jon Lajoie, Lord Gaben, Gordon Freeman}
    
    PS C:\> $xml.SelectNodes("//people//person")
    
    id                                      name                                    age
    --                                      ----                                    ---
    101                                     Jon Lajoie                              22
    102                                     Lord Gaben                              65
    103                                     Gordon Freeman                          29
    
    PS C:\> $xml.SelectSingleNode("people//person[1]//name")
    Jon Lajoie
    
    PS C:\> $xml.SelectSingleNode("people//person[2]//age")
    65

    PS C:\> $xml.SelectSingleNode("people//person[3]//@id")
    103

***Accessing XML containing namespaces with XPath***

    PS C:\> [xml]$xml = @"
    <ns:people xmlns:ns="http://schemas.xmlsoap.org/soap/envelope/">
        <ns:person id="101">
            <ns:name>Jon Lajoie</ns:name>
        </ns:person>
        <ns:person id="102">
            <ns:name>Lord Gaben</ns:name>
        </ns:person>
        <ns:person id="103">
            <ns:name>Gordon Freeman</ns:name>
        </ns:person>
    </ns:people>
    "@
    
    PS C:\> $ns = new-object Xml.XmlNamespaceManager $xml.NameTable
    PS C:\> $ns.AddNamespace("ns", $xml.DocumentElement.NamespaceURI)
    PS C:\> $xml.SelectNodes("//ns:people/ns:person", $ns)
    
    id                                      name
    --                                      ----
    101                                     Jon Lajoie
    102                                     Lord Gaben
    103                                     Gordon Freeman

## Creating an XML Document using XmlWriter()
    # Set The Formatting
    $xmlsettings = New-Object System.Xml.XmlWriterSettings
    $xmlsettings.Indent = $true
    $xmlsettings.IndentChars = "    "
    
    # Set the File Name Create The Document
    $XmlWriter = [System.XML.XmlWriter]::Create("C:\YourXML.xml", $xmlsettings)
    
    # Write the XML Decleration and set the XSL
    $xmlWriter.WriteStartDocument()
    $xmlWriter.WriteProcessingInstruction("xml-stylesheet", "type='text/xsl' href='style.xsl'")
    
    # Start the Root Element
    $xmlWriter.WriteStartElement("Root")
      
        $xmlWriter.WriteStartElement("Object") # <-- Start <Object>
    
            $xmlWriter.WriteElementString("Property1","Value 1")
            $xmlWriter.WriteElementString("Property2","Value 2")
    
            $xmlWriter.WriteStartElement("SubObject") # <-- Start <SubObject> 
                $xmlWriter.WriteElementString("Property3","Value 3")
            $xmlWriter.WriteEndElement() # <-- End <SubObject>
    
        $xmlWriter.WriteEndElement() # <-- End <Object>
    
    $xmlWriter.WriteEndElement() # <-- End <Root> 
    
    # End, Finalize and close the XML Document
    $xmlWriter.WriteEndDocument()
    $xmlWriter.Flush()
    $xmlWriter.Close()

**Output XML File**

    <?xml version="1.0" encoding="utf-8"?>
    <?xml-stylesheet type='text/xsl' href='style.xsl'?>
    <Root>
        <Object>
            <Property1>Value 1</Property1>
            <Property2>Value 2</Property2>
            <SubObject>
                <Property3>Value 3</Property3>
            </SubObject>
        </Object>
    </Root>

## Adding snippits of XML to current XMLDocument
# Sample Data

## XML Document
First, let's define a sample XML document named "**books.xml**" in our current directory:

```XML
<?xml version="1.0" encoding="UTF-8"?>
<books>
    <book>
        <title>Of Mice And Men</title>
        <author>John Steinbeck</author>
        <pageCount>187</pageCount>
        <publishers>
            <publisher>
                <isbn>978-88-58702-15-4</isbn>
                <name>Pascal Covici</name>
                <year>1937</year>
                <binding>Hardcover</binding>
                <first>true</first>
            </publisher>
            <publisher>
                <isbn>978-05-82461-46-8</isbn>
                <name>Longman</name>
                <year>2009</year>
                <binding>Hardcover</binding>
            </publisher>
        </publishers>
        <characters>
            <character name="Lennie Small" />
            <character name="Curley's Wife" />
            <character name="George Milton" />
            <character name="Curley" />
        </characters>
        <film>True</film>
    </book>
    <book>
        <title>The Hunt for Red October</title>
        <author>Tom Clancy</author>
        <pageCount>387</pageCount>
        <publishers>
            <publisher>
                <isbn>978-08-70212-85-7</isbn>
                <name>Naval Institute Press</name>
                <year>1984</year>
                <binding>Hardcover</binding>
                <first>true</first>
            </publisher>
            <publisher>
                <isbn>978-04-25083-83-3</isbn>
                <name>Berkley</name>
                <year>1986</year>
                <binding>Paperback</binding>
            </publisher>
            <publisher>
                <isbn>978-08-08587-35-4</isbn>
                <name>Penguin Putnam</name>
                <year>2010</year>
                <binding>Paperback</binding>
            </publisher>
        </publishers>
        <characters>
            <character name="Marko Alexadrovich Ramius" />
            <character name="Jack Ryan" />
            <character name="Admiral Greer" />
            <character name="Bart Mancuso" />
            <character name="Vasily Borodin" />
        </characters>
        <film>True</film>
    </book>
</books>
```

## New Data
What we want to do is add a few new books to this document, let's say *Patriot Games* by Tom Clancy (yes, I'm a fan of Clancy's works ^__^) and a Sci-Fi favourite: *The Hitchhiker's Guide to the Galaxy* by Douglas Adams mainly because Zaphod Beeblebrox is just fun to read.

Somehow we've acquired the data for the new books and saved them as a list of PSCustomObjects:

```PowerShell
$newBooks = @(
    [PSCustomObject] @{
        "Title" = "Patriot Games";
        "Author" = "Tom Clancy";
        "PageCount" = 540;
        "Publishers" = @(
            [PSCustomObject] @{
                "ISBN" = "978-0-39-913241-4";
                "Year" = "1987";
                "First" = $True;
                "Name" = "Putnam";
                "Binding" = "Hardcover";
            }
        );
        "Characters" = @(
            "Jack Ryan", "Prince of Wales", "Princess of Wales",
            "Robby Jackson", "Cathy Ryan", "Sean Patrick Miller"
        );
        "film" = $True;
    },
    [PSCustomObject] @{
        "Title" = "The Hitchhiker's Guide to the Galaxy";
        "Author" = "Douglas Adams";
        "PageCount" = 216;
        "Publishers" = @(
            [PSCustomObject] @{
                "ISBN" = "978-0-33-025864-7";
                "Year" = "1979";
                "First" = $True;
                "Name" = "Pan Books";
                "Binding" = "Hardcover";
            }
        );
        "Characters" = @(
            "Arthur Dent", "Marvin", "Zaphod Beeblebrox", "Ford Prefect",
            "Trillian", "Slartibartfast", "Dirk Gently"
        );
        "film" = $True;
    }
);

```

## Templates
Now we need to define a few skeleton XML structures for our new data to go into.  Basically, you want to create a skeleton/template for each list of data.  In our example, that means we need a template for the book, characters, and publishers.  We can also use this to define a few default values, such as the value for the `film` tag.

```PowerShell
$t_book = [xml] @'
<book>
    <title />
    <author />
    <pageCount />
    <publishers />
    <characters />
    <film>False</film>
</book>
'@;

$t_publisher = [xml] @'
<publisher>
    <isbn/>
    <name/>
    <year/>
    <binding/>
    <first>false</first>
</publisher>
'@;

$t_character = [xml] @'
<character name="" />
'@;
```

We're done with set-up.

# Adding the new data

Now that we're all set-up with our sample data, let's add the custom objects to the XML Document Object.

```PowerShell
# Read the xml document
$xml = [xml] Get-Content .\books.xml;

# Let's show a list of titles to see what we've got currently:
$xml.books.book | Select Title, Author, @{N="ISBN";E={If ( $_.Publishers.Publisher.Count ) { $_.Publishers.publisher[0].ISBN} Else { $_.Publishers.publisher.isbn}}};;

# Outputs:
# title                                author         ISBN
# -----                                ------         ----
# Of Mice And Men                      John Steinbeck 978-88-58702-15-4
# The Hunt for Red October             Tom Clancy     978-08-70212-85-7

# Let's show our new books as well:
$newBooks | Select Title, Author, @{N="ISBN";E={$_.Publishers[0].ISBN}};

# Outputs:
# Title                                Author        ISBN
# -----                                ------        ----
# Patriot Games                        Tom Clancy    978-0-39-913241-4
# The Hitchhiker's Guide to the Galaxy Douglas Adams 978-0-33-025864-7

# Now to merge the two:

ForEach ( $book in $newBooks ) {
    $root = $xml.SelectSingleNode("/books");
    
    # Add the template for a book as a new node to the root element
    [void]$root.AppendChild($xml.ImportNode($t_book.book, $true));
    
    # Select the new child element
    $newElement = $root.SelectSingleNode("book[last()]");
    
    # Update the parameters of that new element to match our current new book data
    $newElement.title     = [String]$book.Title;
    $newElement.author    = [String]$book.Author;
    $newElement.pageCount = [String]$book.PageCount;
    $newElement.film      = [String]$book.Film;
    
    # Iterate through the properties that are Children of our new Element:
    ForEach ( $publisher in $book.Publishers ) {
        # Create the new child publisher element
        # Note the use of "SelectSingleNode" here, this allows the use of the "AppendChild" method as it returns
        # a XmlElement type object instead of the $Null data that is currently stored in that leaf of the
        # XML document tree
        [void]$newElement.SelectSingleNode("publishers").AppendChild($xml.ImportNode($t_publisher.publisher, $true));
        
        # Update the attribute and text values of our new XML Element to match our new data
        $newPublisherElement = $newElement.SelectSingleNode("publishers/publisher[last()]");
        $newPublisherElement.year = [String]$publisher.Year;
        $newPublisherElement.name = [String]$publisher.Name;
        $newPublisherElement.binding = [String]$publisher.Binding;
        $newPublisherElement.isbn = [String]$publisher.ISBN;
        If ( $publisher.first ) {
            $newPublisherElement.first = "True";
        }
    }
    
    ForEach ( $character in $book.Characters ) {
        # Select the characters xml element
        $charactersElement = $newElement.SelectSingleNode("characters");
        
        # Add a new character child element
        [void]$charactersElement.AppendChild($xml.ImportNode($t_character.character, $true));
        
        # Select the new characters/character element
        $characterElement = $charactersElement.SelectSingleNode("character[last()]");
        
        # Update the attribute and text values to match our new data
        $characterElement.name = [String]$character;
    }
}

# Check out the new XML:
$xml.books.book | Select Title, Author, @{N="ISBN";E={If ( $_.Publishers.Publisher.Count ) { $_.Publishers.publisher[0].ISBN} Else { $_.Publishers.publisher.isbn}}};

# Outputs:
# title                                author         ISBN
# -----                                ------         ----
# Of Mice And Men                      John Steinbeck 978-88-58702-15-4
# The Hunt for Red October             Tom Clancy     978-08-70212-85-7
# Patriot Games                        Tom Clancy     978-0-39-913241-4
# The Hitchhiker's Guide to the Galaxy Douglas Adams  978-0-33-025864-7
```

We can now write our XML to disk, or screen, or web, or wherever!

# Profit
While this may not be the procedure for everyone I found it to help avoid a whole bunch of `[void]$xml.SelectSingleNode("/complicated/xpath/goes[here]").AppendChild($xml.CreateElement("newElementName")` followed by `$xml.SelectSingleNode("/complicated/xpath/goes/here/newElementName") = $textValue`

I think the method detailed in the example is cleaner and easier to parse for normal humans.

# Improvements
It may be possible to change the template to include elements with children instead of breaking out each section as a separate template.  You just have to take care to clone the previous element when you loop through the list.

