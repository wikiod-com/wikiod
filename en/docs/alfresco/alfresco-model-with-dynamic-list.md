---
title: "Alfresco model with dynamic list"
slug: "alfresco-model-with-dynamic-list"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Basic example
**content-model.xml** :

    ....
    <constraints>
        <constraint name="my:aConstraintList" type="x.y.z.project.model.constraint.AConstraintList">
        </constraint>
    ....
    <property name="my:aValue">
        <title>My Value</title>
        <type>d:text</type>
        <constraints>
              <constraint ref="my:aConstraintList"></constraint>
        </constraints>
     </property>

and the Java class declared before :
        
    public class AConstraintList extends ListOfValuesConstraint implements Serializable {

        ....
        @Override
        public final List<String> getAllowedValues() {
            // Return here the list of values. Enum, call a webservice, etc.
        }

         @Override
         public final String getDisplayLabel(final String value, final MessageLookup messageLookup) {
             // Return here the label for the value
         }
    }

