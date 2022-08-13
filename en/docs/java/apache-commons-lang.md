---
title: "Apache Commons Lang"
slug: "apache-commons-lang"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Implement equals() method
To implement the `equals` method of an object easily you could use the `EqualsBuilder` class.

Selecting the fields:

    @Override
    public boolean equals(Object obj) {
    
        if(!(obj instanceof MyClass)) {
            return false;
        }
        MyClass theOther = (MyClass) obj;
        
        EqualsBuilder builder = new EqualsBuilder();
        builder.append(field1, theOther.field1);
        builder.append(field2, theOther.field2);
        builder.append(field3, theOther.field3);
        
        return builder.isEquals();
    }

Using reflection:

    @Override
    public boolean equals(Object obj) {
        return EqualsBuilder.reflectionEquals(this, obj, false);
    }

the boolean parameter is to indicates if the equals should check transient fields.

Using reflection avoiding some fields:

    @Override
    public boolean equals(Object obj) {
        return EqualsBuilder.reflectionEquals(this, obj, "field1", "field2");
    }



## Implement hashCode() method
To implement the `hashCode` method of an object easily you could use the `HashCodeBuilder` class.

Selecting the fields:

    @Override
    public int hashCode() {
        
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(field1);
        builder.append(field2);
        builder.append(field3);
        
        return builder.hashCode();
    }

Using reflection:

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this, false);
    }

the boolean parameter indicates if it should use transient fields.

Using reflection avoiding some fields:

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this, "field1", "field2");
    }




## Implement toString() method
To implement the `toString` method of an object easily you could use the `ToStringBuilder` class.

Selecting the fields:

    @Override
    public String toString() {

        ToStringBuilder builder = new ToStringBuilder(this);
        builder.append(field1);
        builder.append(field2);
        builder.append(field3);
        
        return builder.toString();
    }

Example result:

    ar.com.jonat.lang.MyClass@dd7123[<null>,0,false]


Explicitly giving names to the fields:

    @Override
    public String toString() {

        ToStringBuilder builder = new ToStringBuilder(this);
        builder.append("field1",field1);
        builder.append("field2",field2);
        builder.append("field3",field3);
        
        return builder.toString();
    }

Example result:

    ar.com.jonat.lang.MyClass@dd7404[field1=<null>,field2=0,field3=false]


You could change the style via parameter:

    @Override
    public String toString() {

        ToStringBuilder builder = new ToStringBuilder(this,
                ToStringStyle.MULTI_LINE_STYLE);
        builder.append("field1", field1);
        builder.append("field2", field2);
        builder.append("field3", field3);

        return builder.toString();
    }

Example result:

    ar.com.bna.lang.MyClass@ebbf5c[
      field1=<null>
      field2=0
      field3=false
    ]

There are some styles, for example JSON, no Classname, short, etc ...

Via reflection:

    @Override
    public String toString() {
        return ToStringBuilder.reflectionToString(this);
    }

You could also indicate the style:

    @Override
    public String toString() {
        return ToStringBuilder.reflectionToString(this, ToStringStyle.JSON_STYLE);
    }





