---
title: "Using Lists with Thymeleaf"
slug: "using-lists-with-thymeleaf"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Using list in select
   You can use list variable to form `<select>` elements

     <select th:field="*{countries}">
         <option th:each="country: ${countries}"
                 th:value="${country.id}"
                 th:text="#{${'selected.label.' + country.name}}"/>
    </select>



## Form table


    <table id="countryList">
        <thead>
            <tr>
                <th th:text="#{country.label.name}"> Country </th>
                <th th:text="#{country.label.capital}"> Capital </th>
                <th th:text="#{country.label.square}"> Square </th>
            </tr>
        </thead>
        <tbody>
            <tr th:each="country : ${countryList}">
                <td th:text="${country.name}"></td>
                <td th:text="${country.capital}"></td>
                <td th:text="${country.square}"></td>
            </tr>
        </tbody>
    </table>

