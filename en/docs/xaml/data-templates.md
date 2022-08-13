---
title: "Data templates"
slug: "data-templates"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Using DataTemplate in a ListBox
Suppose we have the following XAML snippet:

    <ListBox x:Name="MyListBox" />

Then in the code-behind for this XAML file, we write the following in the constructor:

    MyListBox.ItemsSource = new[]
    {
        1, 2, 3, 4, 5
    };

Running the application, we get a list of numbers we entered.

[![enter image description here][1]][1]

However, if we try to display a list of objects of a custom type, like this

    MyListBox.ItemsSource = new[]
    {
        new Book { Title = "The Hitchhiker's Guide to the Galaxy", Author = "Douglas Adams" },
        new Book { Title = "The Restaurant at the End of the Universe", Author = "Douglas Adams" },
        new Book { Title = "Life, the Universe and Everything", Author = "Douglas Adams" },
        new Book { Title = "So Long, and Thanks for All the Fish", Author = "Douglas Adams" },
        new Book { Title = "Mostly Harmless", Author = "Douglas Adams" }
    };

assuming we have a class called ```Book```

    public class Book
    {
        public string Title { get; set; }
        public string Author { get; set; }
    }

then the list would look something like this:

[![enter image description here][2]][2]

While we might assume the ListBox will be "smart enough" to display our book objects just right, what we actually see is the full name of the ```Book``` type. What ListBox actually does here is calling the built-in ```ToString()``` method on objects it wants to display, and while that produces the desirable outcome in the case of numbers, calling ```ToString()``` on objects of custom classes results in getting the name of their type, as seen on the screenshot.

We could solve that by writing ```ToString()``` for our book class, i.e.

    public override string ToString()
    {
        return Title;
    }

[![enter image description here][3]][3]

However, that's not very flexible. What if we want to display the author as well? We could write that into the ```ToString``` too, but what if we don't want that in all lists in the app? How about a nice book cover to display?

That's where DataTemplates can help. They are snippets of XAML that can be "instantiated" as needed, filled in with details according to the source data they are created for. Simply put, if we extend our ListBox code as follows:

    <ListBox x:Name="MyListBox">
        <ListBox.ItemTemplate>
            <DataTemplate>
                <TextBlock Text="{Binding Title}" />
            </DataTemplate>
        </ListBox.ItemTemplate>
    </ListBox>

then the list will create a ```TextBox``` for each item in its source, and those TextBoxes will have their ```Text``` properties "filled in" with values from the ```Title``` property of our objects.

If we run the application now, we get the same output as above, *even if we delete the custom ```ToString``` implementation. What's beneficial about this is that we can then customize this template well beyond the capabilities of a simple ```string``` (and ```ToString```). We can use any XAML element we want, including custom ones, and can "bind" their values to actual data from our objects (like ```Title``` in the example above). For example, extend the template as follows:

    <ListBox x:Name="MyListBox">
        <ListBox.ItemTemplate>
            <DataTemplate>
                <StackPanel>
                    <TextBlock FontStyle="Italic" Text="{Binding Author}" />
                    <TextBlock FontSize="18" Text="{Binding Title}" />
                </StackPanel>
            </DataTemplate>
        </ListBox.ItemTemplate>
    </ListBox>

Then we get a nice formatted view of our books!

[![enter image description here][4]][4]

  [1]: http://i.stack.imgur.com/K35hZ.png
  [2]: http://i.stack.imgur.com/weQhp.png
  [3]: http://i.stack.imgur.com/mMVMG.png
  [4]: http://i.stack.imgur.com/syYL1.png

