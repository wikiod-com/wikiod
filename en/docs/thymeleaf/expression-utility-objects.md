---
title: "Expression Utility Objects"
slug: "expression-utility-objects"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Format date
 

    <p>
      Today: <span th:text="${#calendars.format(today,'dd MMMM yyyy')}">30 May 2017</span>
    </p>

## String length
    <div th:if="*{userMessage!=null and #strings.length(userMessage)>0}">
         <label th:text = "*{userMessage}"/>
    </div>

## String contains
    <div th:if="${#strings.contains(#httpServletRequest.requestURI, 'email')}">
        <div th:replace="fragments/email::welcome">
    </div>

## Parsing date
Get year from date

    <p>
      Year: <span th:text="${#dates.year(today)}">2017</span>
    </p>
Get month 

    <p>
      Month number: <span th:text="${#dates.month(today)}">8</span>
      Month: <span th:text="${#dates.monthName(today)}">August</span>
      Month short name: <span th:text="${#dates.monthNameShort(today)}">Aug</span>  
    </p>
Get day

    <p>
      Day: <span th:text="${#dates.day(today)}">26</span>
    </p>
Get day of week

    <p>
        Day: <span th:text="${#dates.dayOfWeek(today)}">1</span>
        Day: <span th:text="${#dates.dayOfWeekName(today)}">Monday</span>
        Day: <span th:text="${#dates.dayOfWeekNameShort(today)}">Mo</span>
    </p>
Get time

    <p>
        Hour: <span th:text="${#dates.hour(today)}">10</span>
        Minute: <span th:text="${#dates.minute(today)}">50</span>
        Second: <span th:text="${#dates.second(today)}">48</span>
        Millisecond: <span th:text="${#dates.millisecond(today)}">48</span>
    <p>

## Format decimal
    <p>
        Order sum: <span th:text="${#numbers.formatDecimal(orderSum, 0, 'COMMA', 2, 'POINT')}">1,145,000.52</span>
    </p>


