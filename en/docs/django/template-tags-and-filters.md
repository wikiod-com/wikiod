---
title: "Template Tags and Filters"
slug: "template-tags-and-filters"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Custom Filters
Filters allows you to apply a function to a variable. This function may take **0** or **1** argument. Here is the syntax:

    {{ variable|filter_name }} 
    {{ variable|filter_name:argument }}

Filters can be chained so this is  perfectly valid:

    {{ variable|filter_name:argument|another_filter }}

If translated to python the above line would give something like this:

    print(another_filter(filter_name(variable, argument)))

In this example, we will write a custom filter `verbose_name` that applies to a Model (instance or class) or a QuerySet. It will return the verbose name of a model, or its verbose name plural if the argument is set to `True`.

    @register.filter
    def verbose_name(model, plural=False):
        """Return the verbose name of a model.
        `model` can be either:
          - a Model class
          - a Model instance
          - a QuerySet
          - any object refering to a model through a `model` attribute.

        Usage:
          - Get the verbose name of an object
              {{ object|verbose_name }}
          - Get the plural verbose name of an object from a QuerySet
              {{ objects_list|verbose_name:True }}
        """
        if not hasattr(model, '_meta'):
            # handle the case of a QuerySet (among others)
            model = model.model
        opts = model._meta
        if plural:
            return opts.verbose_name_plural
        else:
            return opts.verbose_name


        

## Simple tags
The simplest way of defining a custom template tag is to use a `simple_tag`. These are very straightforward to setup. The function name will be the tag name (though you can override it), and arguments will be tokens ("words" separated by spaces, except spaces enclosed between quotes). It even supports keyword arguments.

Here is a useless tag that will illustrate our example:

    {% useless 3 foo 'hello world' foo=True bar=baz.hello|capfirst %}

Let `foo` and `baz` be context variables like the following:

    {'foo': "HELLO", 'baz': {'hello': "world"}}

Say we want this very useless tag to render like this:

    HELLO;hello world;bar:World;foo:True<br/>
    HELLO;hello world;bar:World;foo:True<br/>
    HELLO;hello world;bar:World;foo:True<br/>

Kind of arguments concatenation repeated 3 times (3 being the first argument).

Here is what the tag implementation may look like:

    from django.utils.html import format_html_join

    @register.simple_tag
    def useless(repeat, *args, **kwargs):
        output = ';'.join(args + ['{}:{}'.format(*item) for item in kwargs.items()])
        outputs = [output] * repeat
        return format_html_join('\n', '{}<br/>', ((e,) for e in outputs))
        
`format_html_join` allows to mark `<br/>` as safe HTML, but not the content of `outputs`.

## Advanced custom tags using Node
Sometimes what you want to do is just too complex for a `filter` or a `simple_tag`. Fow this you will need to create a compilation function and a renderer.

In this example we will create a template tag `verbose_name` with the following syntax:

 Example | Description |
| ------- | ----------- |
| `{% verbose_name obj %}` | Verbose name of a model |
| `{% verbose_name obj 'status' %}` | Verbose name of the field "status" |
| `{% verbose_name obj plural %}` | Verbose name plural of a model |
| `{% verbose_name obj plural capfirst %}` | Capitalized verbose name plural of a model |
| `{% verbose_name obj 'foo' capfirst %}` | Capitalized verbose name of a field |
| `{% verbose_name obj field_name %}` | Verbose name of a field from a variable |
| <code>{% verbose_name obj 'foo'&#124;add:'_bar' %}</code> | Verbose name of a field "foo_bar" |

The reason why we can't do this with a simple tag is that `plural` and `capfirst` are neither variables nor strings, they are "keywords". We could obviously decide to pass them as strings `'plural'` and `'capfirst'`, but it may conflict with fields with these names. Would `{% verbose_name obj 'plural' %}` mean "verbose name plural of `obj`" or "verbose name of `obj.plural`"?

First let's create the compilation function:

    @register.tag(name='verbose_name')
    def do_verbose_name(parser, token):
        """
        - parser: the Parser object. We will use it to parse tokens into
                  nodes such as variables, strings, ...
        - token: the Token object. We will use it to iterate each token
                 of the template tag.
        """
        # Split tokens within spaces (except spaces inside quotes)
        tokens = token.split_contents()
        tag_name = tokens[0]
        try:
            # each token is a string so we need to parse it to get the actual
            # variable instead of the variable name as a string.
            model = parser.compile_filter(tokens[1])
        except IndexError:
            raise TemplateSyntaxError(
                "'{}' tag requires at least 1 argument.".format(tag_name))

        field_name = None
        flags = {
            'plural': False,
            'capfirst': False,
        }

        bits = tokens[2:]
        for bit in bits:
            if bit in flags.keys():
                # here we don't need `parser.compile_filter` because we expect
                # 'plural' and 'capfirst' flags to be actual strings.
                if flags[bit]:
                    raise TemplateSyntaxError(
                        "'{}' tag only accept one occurrence of '{}' flag".format(
                            tag_name, bit)
                    )
                flags[bit] = True
                continue
            if field_name:
                raise TemplateSyntaxError((
                    "'{}' tag only accept one field name at most. {} is the second "
                    "field name encountered."
                ).format(tag_name, bit)
            field_name = parser.compile_filter(bit)

        # VerboseNameNode is our renderer which code is given right below
        return VerboseNameNode(model, field_name, **flags)

And now the renderer:

    class VerboseNameNode(Node):

        def __init__(self, model, field_name=None, **flags):
            self.model = model
            self.field_name = field_name
            self.plural = flags.get('plural', False)
            self.capfirst = flags.get('capfirst', False)

        def get_field_verbose_name(self):
            if self.plural:
                raise ValueError("Plural is not supported for fields verbose name.")
            return self.model._meta.get_field(self.field_name).verbose_name

        def get_model_verbose_name(self):
           if self.plural:
               return self.model._meta.verbose_name_plural
           else:
               return self.model._meta.verbose_name

        def render(self, context):
            """This is the main function, it will be called to render the tag.
            As you can see it takes context, but we don't need it here.
            For instance, an advanced version of this template tag could look for an
            `object` or `object_list` in the context if `self.model` is not provided.
            """
            if self.field_name:
                verbose_name = self.get_field_verbose_name()
            else:
                verbose_name = self.get_model_verbose_name()
            if self.capfirst:
                verbose_name = verbose_name.capitalize()
            return verbose_name


