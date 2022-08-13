---
title: "TroubleShooting"
slug: "troubleshooting"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## ERROR: Too many ROS containers exist for the following projections:
it caused by your batch size too small, which lead to a lot of ROS Containers created and reach the limitation(1024 default). you should do defragment using TupleMover task(mergeout) before the error raised.

To do troubleshooting:
1. ROS Containers viewed from the projections.


    select * from STORAGE_CONTAINERS where projection_name like '%DATASET_TABLE%';

2. check ContainersPerProjectionLimit settings Views


    SELECT *
    FROM CONFIGURATION_PARAMETERS
    WHERE parameter_name = 'ContainersPerProjectionLimit' ;

3. ROS Container Number Lookup


    select count(*) from STORAGE_CONTAINERS where projection_name like '%DATASET_TABLE%';

4. Solutions


    -- change ContainersPerProjectionLimit settings
    SELECT SET_CONFIG_PARAMETER('ContainersPerProjectionLimit', 2048);

or

    -- change mergeout frequency
    SELECT SET_CONFIG_PARAMETER('MergeOutInterval', 30);

or

    -- Do TupleMover Task manually
    select do_tm_task('mergeout','projection_name')






## COPY Rejected Data and Exception Files
it often happened some rows with format issue, data type issue rejected by copy command while try load it by copy command. the query return succeed but some of data rejected.

To do troubleshooting
1. save Rejected Data and Exceptions


    COPY large_tbl FROM :file1 ON site01,
                   :file2 ON site01,
                   :file3 ON site02,
                   :file4 ON site02
                   DELIMITER '|'
                   REJECTED DATA :reject_s1 ON site01, :reject_s2 ON site02 
                   EXCEPTIONS :except_s1 ON site01, :except_s2 ON site02;

2. check errors in exceptions file, and fix it one by one





