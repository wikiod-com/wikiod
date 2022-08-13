---
title: "Select nodes based on their children"
slug: "select-nodes-based-on-their-children"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Select nodes based on child count
_Sample XML_

     <Students>
        <Student>
            <Name>
                <First>Ashley</First>
                <Last>Smith</Last>
            </Name>
            <Grades>
                <Exam1>A</Exam1>
                <Exam2>B</Exam2>
                <Final>A</Final>
            </Grades>
        </Student>
        <Student>
            <Name>
                <First>Bill</First>
                <Last>Edwards</Last>
            </Name>
            <Grades>
                <Exam1>A</Exam1>
            </Grades>
        </Student>    
    </Students>

_XPath_

Select all students that have at least 2 grades recorded

    //Student[count(./Grades/*) > 1]

_Output_

    <Student>
        <Name>
            <First>Ashley</First>
            <Last>Smith</Last>
        </Name>
        <Grades>
            <Exam1>A</Exam1>
            <Exam2>B</Exam2>
            <Final>A</Final>
        </Grades>
    </Student>

## Select nodes based on specific child node
_Sample XML_

     <Students>
        <Student>
            <Name>
                <First>Ashley</First>
                <Last>Smith</Last>
            </Name>
            <Grades>
                <Exam1>A</Exam1>
                <Exam2>B</Exam2>
                <Final>A</Final>
            </Grades>
        </Student>
        <Student>
            <Name>
                <First>Bill</First>
                <Last>Edwards</Last>
            </Name>
            <Grades>
                <Exam1>A</Exam1>
            </Grades>
        </Student>    
    </Students>

_XPath_

Select all students that have a score for Exam2 recorded

    //Student[./Grades/Exam2]

or

    //Student[.//Exam2]

_Output_

    <Student>
        <Name>
            <First>Ashley</First>
            <Last>Smith</Last>
        </Name>
        <Grades>
            <Exam1>A</Exam1>
            <Exam2>B</Exam2>
            <Final>A</Final>
        </Grades>
    </Student>

