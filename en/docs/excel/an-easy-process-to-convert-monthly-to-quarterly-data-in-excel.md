---
title: "An easy process to convert Monthly to Quarterly data in Excel"
slug: "an-easy-process-to-convert-monthly-to-quarterly-data-in-excel"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

A frequent desire is to convert monthly data into quarterly data format. One simple method is to 1) sum the appropriate months into quarterly sums, then 2) identify those months as quarters, and finally 3) filter the quarterly data out of your monthly data. Here's a relatively quick and easy method illustrated through the following example:

## An easy way to perform a sometimes tedious routine task:
If you have your data with each month of data arranged in rows like so:

   


[![Example Data][1]][1] 


start by creating quarterly sums in the adjacent columns, D & E in our example.

Start with the third row in the new column or the first quarter you want to create, in this example we'll use March 31st (2000-03-31). Use the summation function over the prior two months and the quarter month, for March use:

**=SUM(B2:B4)**

Then use a similar function for the total in the next column E. It will have the following formula:

**=SUM(C2:C4)**

Don't concern yourself that the sums are created for months that are not quarters. This is done to make the copying easier for you. You'll not need those values anyway in the next few steps.

Finally, we need to identify quarters without a lot of work. If your Date column isn't in an Excel Date format, such as text, you may want to convert it. In a new column, F in this example, begin this formula in the first row of data. We will mark the rows using the following formula:

**=IF(MOD(MONTH(A2),3)=0, "Quarter", "Month")**

Now copy the three formulas in the new columns D,E & F into the remaining cells in your table. It should look like this:


[![Sample Data with formulas added to three columns][2]][2]


Finally, select this table, choose **Format As Table**, pick a simple format, and Excel will produce a result like so:


[![enter image description here][3]][3]


The final step is to select the **filter button** over the new "Period" column in F and select only the "Quarters" values to filter on:

[![Filter Menu Choice on Period column][4]][4]


Your table should look like this now:

[![Filtered Quarterly Data][5]][5]


  [1]: https://i.stack.imgur.com/mqYp5.png
  [2]: https://i.stack.imgur.com/Q0Ub2.png
  [3]: https://i.stack.imgur.com/OPHlK.png
  [4]: https://i.stack.imgur.com/UZSrF.png
  [5]: https://i.stack.imgur.com/QkrO7.png

At this point I would suggest selecting only the columns you need from your final table and then using **Paste->Special->Values** to relocate your data.



