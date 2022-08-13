---
title: "Index configuration"
slug: "index-configuration"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Field Configuration
When adding custom fields to a Lucene index you can add new fields into the index using the following configuration:

<!-- language: xml -->

     <configuration ref="contentSearch/indexConfigurations/defaultLuceneIndexConfiguration">
      <indexAllfields>false</indexAllfields>
      <fieldNames hint="raw:AddFieldByFieldName">
          <field fieldName="title" storageType="YES" indexType="TOKENIZED"   vectorType="NO" boost="1f" type="System.String" 
                 settingType="Sitecore.ContentSearch.LuceneProvider.LuceneSearchFieldConfiguration, Sitecore.ContentSearch.LuceneProvider"/>
      </fieldNames>
    </configuration>

A field has a couple of possible properties:
 - `storageType`
 - `indexType`
 - `vectorType`
 - `boost`

Tjhese fields directly relate to the content of the `Sitecore.ContentSearch.LuceneProvider.LuceneSearchFieldConfiguration` class. If we reflect the values out of this class we can see their possible values, etc.

StorageType
-----------
<!-- language: c# -->
    /// <summary>Specifies whether and how a field should be stored. </summary>
    public enum Store
    {
        /// <summary>Store the original field value in the index. This is useful for short texts
        /// like a document's title which should be displayed with the results. The
        /// value is stored in its original form, i.e. no analyzer is used before it is
        /// stored.
        /// </summary>
        YES,
        /// <summary>Do not store the field value in the index. </summary>
        NO
    }

IndexType
---------
<!-- language: c# -->
    /// <summary>Specifies whether and how a field should be indexed. </summary>
    public enum Index
    {
        /// <summary>Do not index the field value. This field can thus not be searched,
        /// but one can still access its contents provided it is
        /// <see cref="T:Lucene.Net.Documents.Field.Store">stored</see>. 
        /// </summary>
        NO,
        /// <summary>Index the tokens produced by running the field's
        /// value through an Analyzer.  This is useful for
        /// common text. 
        /// </summary>
        ANALYZED,
        /// <summary>Index the field's value without using an Analyzer, so it can be searched.
        /// As no analyzer is used the value will be stored as a single term. This is
        /// useful for unique Ids like product numbers.
        /// </summary>
        NOT_ANALYZED,
        /// <summary>Expert: Index the field's value without an Analyzer,
        /// and also disable the storing of norms.  Note that you
        /// can also separately enable/disable norms by calling
        /// <see cref="!:AbstractField.SetOmitNorms" />.  No norms means that
        /// index-time field and document boosting and field
        /// length normalization are disabled.  The benefit is
        /// less memory usage as norms take up one byte of RAM
        /// per indexed field for every document in the index,
        /// during searching.  Note that once you index a given
        /// field <i>with</i> norms enabled, disabling norms will
        /// have no effect.  In other words, for this to have the
        /// above described effect on a field, all instances of
        /// that field must be indexed with NOT_ANALYZED_NO_NORMS
        /// from the beginning. 
        /// </summary>
        NOT_ANALYZED_NO_NORMS,
        /// <summary>Expert: Index the tokens produced by running the
        /// field's value through an Analyzer, and also
        /// separately disable the storing of norms.  See
        /// <see cref="F:Lucene.Net.Documents.Field.Index.NOT_ANALYZED_NO_NORMS" /> for what norms are
        /// and why you may want to disable them. 
        /// </summary>
        ANALYZED_NO_NORMS
    }

VectorType
----------
<!-- language: c# -->
    /// <summary>Specifies whether and how a field should have term vectors. </summary>
    public enum TermVector
    {
        /// <summary>Do not store term vectors. </summary>
        NO,
        /// <summary>Store the term vectors of each document. A term vector is a list
        /// of the document's terms and their number of occurrences in that document. 
        /// </summary>
        YES,
        /// <summary> Store the term vector + token position information
        ///
        /// </summary>
        /// <seealso cref="F:Lucene.Net.Documents.Field.TermVector.YES">
        /// </seealso>
        WITH_POSITIONS,
        /// <summary> Store the term vector + Token offset information
        ///
        /// </summary>
        /// <seealso cref="F:Lucene.Net.Documents.Field.TermVector.YES">
        /// </seealso>
        WITH_OFFSETS,
        /// <summary> Store the term vector + Token position and offset information
        ///
        /// </summary>
        /// <seealso cref="F:Lucene.Net.Documents.Field.TermVector.YES">
        /// </seealso>
        /// <seealso cref="F:Lucene.Net.Documents.Field.TermVector.WITH_POSITIONS">
        /// </seealso>
        /// <seealso cref="F:Lucene.Net.Documents.Field.TermVector.WITH_OFFSETS">
        /// </seealso>
        WITH_POSITIONS_OFFSETS
    }

Boost
-----

Add's a [boost value](https://www.wikiod.com/lucene/queries#Boosting queries) to the Lucene index for this item

