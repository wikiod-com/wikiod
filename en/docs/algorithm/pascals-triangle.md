---
title: "Pascal's Triangle"
slug: "pascals-triangle"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Pascal triangle in C
    int i, space, rows, k=0, count = 0, count1 = 0;
    row=5;
    for(i=1; i<=rows; ++i)
    {
        for(space=1; space <= rows-i; ++space)
        {
            printf("  ");
            ++count;
        }

        while(k != 2*i-1)
        {
            if (count <= rows-1)
            {
                printf("%d ", i+k);
                ++count;
            }
            else
            {
                ++count1;
                printf("%d ", (i+k-2*count1));
            }
            ++k;
        }
        count1 = count = k = 0;

        printf("\n");
    }


**Output**

            1
          2 3 2
        3 4 5 4 3
      4 5 6 7 6 5 4
    5 6 7 8 9 8 7 6 5

## Pascal's Triagle Basic Information
One of the most interesting Number Patterns is [Pascal's Triangle][1]. The Name "Pascal's Triangle" named after [Blaise Pascal][2], a famous French Mathematician and Philosopher.

In Mathematics, Pascal's Triangle is a triangular array of binomial coefficients.The rows of Pascal's triangle are conventionally enumerated starting with row n = 0 at the top (the 0th row). The entries in each row are numbered from the left beginning with k = 0 and are usually staggered relative to the numbers in the adjacent rows.

**The triangle is constructed in the below manner:**

 - In the topmost row, there is a unique nonzero entry 1.
 - Each entry of each subsequent row is constructed by adding the number above and to the left with the number above and to the right, treating blank entries as 0.

For example, the initial number in the first (or any other) row is 1 (the sum of 0 and 1), whereas the numbers 1 and 3 in the third row are added to produce the number 4 in the fourth row.

**Equation to generate each entry in Pascal triangle:**

[![Pascal Equation][3]][3]

for any non-negative integer n and any integer k between 0 and n, inclusive. This recurrence for the binomial coefficients is known as [Pascal's rule][4]. Pascal's triangle has higher dimensional generalizations. The three-dimensional version is called Pascal's pyramid or Pascal's tetrahedron, while the general versions are called Pascal's simplices.

**Example of Pascal's Triangle:**

[![Pascal's Triangle Example][5]][5]

  [1]: https://en.wikipedia.org/wiki/Pascal%27s_triangle
  [2]: https://en.wikipedia.org/wiki/Blaise_Pascal
  [3]: https://i.stack.imgur.com/7WEJ9.png
  [4]: https://en.wikipedia.org/wiki/Pascal%27s_rule
  [5]: https://i.stack.imgur.com/chhsE.gif

## Implementation of Pascal's Triangle in C#
    public class PascalsTriangle
    {
        static void PascalTriangle(int n)
        {
            for (int line = 1; line <= n; line++)
            {
                int c = 1;
                for (int i = 1; i <= line; i++)
                {
                    Console.WriteLine(c);
                    c = c * (line - i) / i;
                }
                Console.WriteLine("\n");
            }
        }

        public static int Main(int input)
        {
            PascalTriangle(input);
            return input;
        }
    }

