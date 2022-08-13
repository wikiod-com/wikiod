---
title: "Database Query DataProcessor - Examples"
slug: "database-query-dataprocessor---examples"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## How to get selected records from another table (using TCA group field)
> Example of a TCA field configuration where you can select records from a table

    'my_topfeatures' => array(
            'label' => 'Select Topfeatures',
            'config' => array(
                'type' => 'group',
                'internal_type' => 'db',
                'size' => '4',
                'allowed' => 'tx_topfeatures_items',
                'wizards' => array(
                    'suggest' => array(
                        'type' => 'suggest',
                        'default' => array(
                            'searchWholePhrase' => 1
                        ),
                        'pages' => array(
                            'searchCondition' => 'doktype = 1'
                        )
                    )
                )
            )
        ) 

> Typoscript setup:

    tt_content.yourctype {
        templateName = FE_Topfeatures
        dataProcessing {
            10 = TYPO3\CMS\Frontend\DataProcessing\DatabaseQueryProcessor
            10 {
                table = tx_topfeatures_items 
                pidInList = root
                recursive = 1
                where = deleted = 0 AND hidden = 0
                where.inval = 1
                uidInList = ###selectedtopfeatures###
                orderBy = sorting
                as = tx_topfeatures_items
                markers {
                    selectedtopfeatures.field = my_topfeatures
                    selectedtopfeatures.commaSeparatedList = 1
                }
    
                dataProcessing {
                    10 = TYPO3\CMS\Frontend\DataProcessing\FilesProcessor
                    10 {
                        references.fieldName = downloadfile
                        as = downloadfiles
                    }
                }
            }
        }
    }


