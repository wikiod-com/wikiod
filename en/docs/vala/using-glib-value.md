---
title: "Using GLib.Value"
slug: "using-glibvalue"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## How to initialize it ?
To initialize struct, you can do like this :

<!-- language: lang-vala -->
    public static void main (string[] args) {
        Value val = Value (typeof (int));
        val.set_int (33);
    }

But Vala brings another way to initialize values :

<!-- language: lang-vala -->
    public static void main (string[] args) {
        Value val = 33;
    }

Your value is initialized with 'int' type and holds '33' int value. 

## How to use it ?
Use one of GLib.Value get methods ([see valadoc documentation](https://valadoc.org/gobject-2.0/GLib.Value.html)) or cast your value with the type of your value :

<!-- language: lang-vala -->
    public static void main (string[] args) {
        Value val = 33;
        int i = val.get_int();
        int j = (int)val;
    }

Note : if your current value doesn't contain desired type, GObject system will throw critical error :

<!-- language: lang-vala -->
    public static void main (string[] args) {
        Value val = 33;
        string s = (string)val;
    }

```bash
(process:5725): GLib-GObject-CRITICAL **: g_value_get_string: assertion 'G_VALUE_HOLDS_STRING (value)' failed
```

## Use GLib.Value in function parameters
This exemple shows how you can pass several types in function parameters :

<!-- language: lang-vala -->
    static void print_value (Value val) {
        print ("value-type : %s\n", val.type().name());
        print ("value-content : %s\n\n", val.strdup_contents());
    }

    public static void main (string[] args) {
        print_value (33);
        print_value (24.46);
        print_value ("string");
    }

```
value-type : gint
value-content : 33

value-type : gdouble
value-content : 24.460000

value-type : gchararray
value-content : "string"

```
Note : if GObject can [transform](https://valadoc.org/gobject-2.0/GLib.Value.transform.html) your value with 'string' type (gchararray), 'strdup_contents' returns converted value, instead of pointer adress

<!-- language: lang-vala -->
    static void print_value (Value val) {
        print ("value-type : %s\n", val.type().name());
        print ("value-content : %s\n\n", val.strdup_contents());
    }

    public static void main (string[] args) {
        print_value (new DateTime.now_local());
    }

```
value-type : GDateTime
value-content : ((GDateTime*) 0x560337def040)
```

## Register types for GLib.Value
In previous example, Value.strdup_contents prints GLib.DateTime as pointer address.
You can register functions that will transform value to desired type. First, create a function that will have [this](https://valadoc.org/gobject-2.0/GLib.ValueTransform.html) signature :

<!-- language: lang-vala -->
    static void datetime_to_string (Value src_value, ref Value dest_value) {
        DateTime dt = (DateTime)src_value;
        dest_value.set_string (dt.to_string());
    }

then register this function with [Value.register_transform_func](https://valadoc.org/gobject-2.0/GLib.Value.register_transform_func.html) :

<!-- language: lang-vala -->
    Value.register_transform_func (typeof (DateTime), typeof (string), datetime_to_string);

now GObject can convert any DateTime object to string value. 

the complete example :

<!-- language: lang-vala -->
    static void datetime_to_string (Value src_value, ref Value dest_value) {
        DateTime dt = (DateTime)src_value;
        dest_value.set_string (dt.to_string());
    }

    static void print_value (Value val) {
        print ("value-type : %s\n", val.type().name());
        print ("value-content : %s\n\n", val.strdup_contents());
    }

    public static void main (string[] args) {
        print_value (new DateTime.now_local());
        Value.register_transform_func (typeof (DateTime), typeof (string), datetime_to_string);
        print_value (new DateTime.now_local());
    }

```
value-type : GDateTime
value-content : ((GDateTime*) 0x560337def040)

value-type : GDateTime
value-content : 2017-04-20T18:40:20+0200


```

