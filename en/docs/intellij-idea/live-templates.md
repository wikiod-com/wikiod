---
title: "Live Templates"
slug: "live-templates"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Add a test method easily
    @org.junit.Test
    public void should_$name$() {
        $END$
    }

Make sure to check the **Shorted FQ names** box when creating this template.

[![configure live template][1]][1]

When you type "should" (the abbreviation),
this will add the necessary `import org.junit.Test;` statement at the top of the file, and this code:

    @Test
    public void should_() {
        
    }

It is thanks to the **Shorten FQ names** option that `@org.junit.Test` is reduced to simply `@Test`.

The `$name$` variable is irrelevant, it could be named something else.
The purpose of that variable is that when the template is inserted in the class,
the cursor will be placed in the position of `$name$`,
asking you to enter something.

After you entered a value for `$name$` (effectively the name of the test method),
the cursor will finally jump to `$END$`, a built-in variable,
so that you can carry on and implement the test case.

  [1]: http://i.stack.imgur.com/Nwcas.png

## Insert the name of the current class
Consider the *utility class pattern*:
a class with only `static` methods and no fields.
It's recommended to prevent instantiation of such classes by adding a private a constructor.

This live template example makes it easy to add a private constructor to an existing class, using the name of the enclosing class.

    private $className$() {
        throw new AssertionError("utility class, forbidden constructor");
    }

Applicable in Java: declaration scope.

[![configure live template][1]][1]

Click **Edit variables** to define the `className` variable as the built-in `className()` expression, and check the **Skip if defined** box to avoid prompting for a custom name, which is unnecessary in this example.

[![define className variable][2]][2]

For example, inside a class like this:

    class ListUtils {

        // ...
    }

When you type "utility_class" (the abbreviation), this will insert a constructor like this:

    class ListUtils {
        private ListUtils() {
            throw new AssertionError("utility class, forbidden constructor");
        }

        // ...
    }



  [1]: http://i.stack.imgur.com/0ip0F.png
  [2]: http://i.stack.imgur.com/lpLSJ.png


