---
title: "How to create a custom fieldType"
slug: "how-to-create-a-custom-fieldtype"
draft: false
images: []
weight: 9904
type: docs
toc: true
---

**Remarks on custom Java class based field:**

This is a small section of a large article [custom sorting in Solr using external field][1] written to sort Solr documents based on custom field comparator.

**Remarks on custom field created from existing Solr fields:**

Apache has created a detailed documentation on this topic - [Understanding Analyzers, Tokenizers, and Filters][2].


  [1]: http://sujitpal.blogspot.in/2011/05/custom-sorting-in-solr-using-external.html
  [2]: https://cwiki.apache.org/confluence/display/solr/Understanding+Analyzers%2C+Tokenizers%2C+and+Filters

## Create a custom Solr field type from own custom Java class
**Schema changes:**

You will need to define a new field type in your solr schema file and then you can create fields of that type. Example schema snippet:

    <!-- Source: solr/example/.../conf/schema.xml -->
    <?xml version="1.0" encoding="UTF-8" ?>
    <schema name="adam" version="1.3">
      <types>
        ...
        <fieldType name="rank_t" class="org.apache.solr.schema.ext.RankFieldType"/>
      </types>
     <fields>
       ...
       <field name="rank" type="rank_t" indexed="true" stored="true"/>
     </fields>
     ...
    </schema>

**Java class for custom field type:**

    // Source: src/java/org/apache/solr/schema/ext/RankFieldType.java
    package org.apache.solr.schema.ext;

    import java.io.IOException;

    import org.apache.lucene.document.Fieldable;
    import org.apache.lucene.search.SortField;
    import org.apache.solr.response.TextResponseWriter;
    import org.apache.solr.schema.FieldType;
    import org.apache.solr.schema.SchemaField;
    import org.apache.solr.search.ext.RankFieldComparatorSource;

    public class RankFieldType extends FieldType {

        @Override
        public SortField getSortField(SchemaField field, boolean top) {
            return new SortField(field.getName(), new RankFieldComparatorSource(), top);
        }

        @Override
        // copied verbatim from GeoHashField method
        public void write(TextResponseWriter writer, String name, Fieldable f) throws IOException {
             writer.writeStr(name, f.stringValue(), false);
        }
    }


## Create custom field type from available field types
Let's get some theoretical knowledge before moving to the example. There are three important terms being used here [Analyzers][1], [Tokenizers][2], and [Filters][3]. To create such custom field you will need to create an analyzer with one tokenizer and one or more filters. As mentioned [here][4], you can have only one tokenizer per analyzer but there are ways to overcome this limitation.



    <fieldType name="alphaOnlySort" class="solr.TextField" sortMissingLast="true" omitNorms="true">
      <analyzer>
        <tokenizer class="solr.KeywordTokenizerFactory"/>
        <filter class="solr.LowerCaseFilterFactory"/>
        <filter class="solr.TrimFilterFactory"/>
        <filter class="solr.PatternReplaceFilterFactory" replace="all" replacement="" pattern="([^a-z])"/>
      </analyzer>
    </fieldType>

Another example:

    <fieldType name="lowercase_text" class="solr.TextField" positionIncrementGap="150">
      <analyzer>
         <tokenizer class="solr.KeywordTokenizerFactory" />
         <filter class="solr.LowerCaseFilterFactory" />
      </analyzer>
    </fieldType>

One more example with description:

    <fieldType name="text_stem" class="solr.TextField">
      <analyzer>
        <tokenizer class="solr.StandardTokenizerFactory"/>
        <filter class="solr.StandardFilterFactory"/>
        <filter class="solr.LowerCaseFilterFactory"/>
        <filter class="solr.EnglishPorterFilterFactory"/>
      </analyzer>
    </fieldType>

This example starts with Solr's standard tokenizer, which breaks the field's text into tokens. Those tokens then pass through Solr's standard filter, which removes dots from acronyms, and performs a few other common operations. All the tokens are then set to lowercase, which will facilitate case-insensitive matching at query time.
The last filter in the above example is a stemmer filter that uses the Porter stemming algorithm. A stemmer is basically a set of mapping rules that maps the various forms of a word back to the base, or stem, word from which they derive. For example, in English the words "hugs", "hugging" and "hugged" are all forms of the stem word "hug". The stemmer will replace all of these terms with "hug", which is what will be indexed. This means that a query for "hug" will match the term "hugged", but not "huge".

Example usage of such custom field:

    <field name="keywords" type="text_stem" indexed="true" stored="true" />

List of available tokenizer types: [list of tokenizer types][5]

List of available filter types: [list of filter types][6]


  [1]: https://cwiki.apache.org/confluence/display/solr/Analyzers
  [2]: https://cwiki.apache.org/confluence/display/solr/About+Tokenizers
  [3]: https://cwiki.apache.org/confluence/display/solr/About+Filters
  [4]: http://stackoverflow.com/a/35516450/3896066
  [5]: https://cwiki.apache.org/confluence/display/solr/Tokenizers
  [6]: https://cwiki.apache.org/confluence/display/solr/Filter+Descriptions

