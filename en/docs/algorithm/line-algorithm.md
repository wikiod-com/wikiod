---
title: "Line Algorithm"
slug: "line-algorithm"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

Line drawing is accomplished by calculating intermediate positions along the line path between
two specified endpoint positions. An output device is then directed to fill in these positions
between the endpoints.

## Bresenham Line Drawing Algorithm
Background Theory:
Bresenham’s Line Drawing Algorithm is an efficient and accurate raster line generating algorithm developed by Bresenham. It involves only integer calculation so it is accurate and fast. It can also be extended to display circles another curves.

In Bresenham line drawing algorithm:<br> 

For Slope |m|<1:<br> 
Either value of x is increased<br> 
OR both x and y is increased using decision parameter.  <br>

For Slope |m|>1:<br>
Either value of y is increased<br> 
OR both x and y is increased using decision parameter.  <br>

**Algorithm for slope |m|<1:<br>**

 1. Input two end points (x1,y1) and (x2,y2) of the line.
 2. Plot the first point (x1,y1).
 3. Calculate<br>
    Delx =| x2 – x1 |
<br>    Dely = | y2 – y1 |
 
 4. Obtain the initial decision parameter as <br>
P = 2 * dely – delx
  
 5. For I = 0 to delx in step of 1<br><br>
     If p < 0 then<br>
                X1 = x1 + 1<br>
                Pot(x1,y1)<br>
                P = p+ 2*dely<br><br>
Else<br>
                X1 = x1 + 1<br>
                Y1 = y1 + 1<br>
                Plot(x1,y1)<br>
                P = p + 2*dely – 2 * delx<br><br>
End if<br><br>
End for<br>
 6. END
 
**Source Code:**

    /* A C program to implement Bresenham line drawing algorithm for |m|<1 */
    #include<stdio.h>
    #include<conio.h>
    #include<graphics.h>
    #include<math.h>

    int main()
    {    
     int gdriver=DETECT,gmode;
     int x1,y1,x2,y2,delx,dely,p,i;
     initgraph(&gdriver,&gmode,"c:\\TC\\BGI");

    printf("Enter the intial points: ");
    scanf("%d",&x1);
    scanf("%d",&y1);
    printf("Enter the end points: ");
    scanf("%d",&x2);
    scanf("%d",&y2);

    putpixel(x1,y1,RED);

    delx=fabs(x2-x1);
    dely=fabs(y2-y1);
    p=(2*dely)-delx;
    for(i=0;i<delx;i++){
    if(p<0)
    {
     x1=x1+1;
     putpixel(x1,y1,RED);
     p=p+(2*dely);
    }
    else
    {
     x1=x1+1;
     y1=y1+1;
     putpixel(x1,y1,RED);
     p=p+(2*dely)-(2*delx);
    }
    }
     getch();
     closegraph();
     return 0;
    }

**Algorithm for slope |m|>1:**
1) Input two end points (x1,y1) and (x2,y2) of the line.
2) Plot the first point (x1,y1).
3) Calculate<br>
    Delx =| x2 – x1 |<br>
    Dely = | y2 – y1 |
4) Obtain the initial decision parameter as
  <br>P = 2 * delx – dely
5) For I = 0 to dely in step of 1<br><br>
If p < 0 then<br>
                y1 = y1 + 1<br>
                Pot(x1,y1)<br>
                P = p+ 2*delx<br><br>
Else<br>
                X1 = x1 + 1<br>
                Y1 = y1 + 1<br>
                Plot(x1,y1)<br>
                P = p + 2*delx – 2 * dely<br><br>
End if<br><br>
End for<br><br>
6) END

**Source Code:**

    /* A C program to implement Bresenham line drawing algorithm for |m|>1 */
    #include<stdio.h>
    #include<conio.h>
    #include<graphics.h>
    #include<math.h>
    int main()
    {
    int gdriver=DETECT,gmode;
    int x1,y1,x2,y2,delx,dely,p,i;
    initgraph(&gdriver,&gmode,"c:\\TC\\BGI");
    printf("Enter the intial points: ");
    scanf("%d",&x1);
    scanf("%d",&y1);
    printf("Enter the end points: ");
    scanf("%d",&x2);
    scanf("%d",&y2);
    putpixel(x1,y1,RED);
    delx=fabs(x2-x1);
    dely=fabs(y2-y1);
    p=(2*delx)-dely;
    for(i=0;i<delx;i++){
    if(p<0)
    {
    y1=y1+1;
    putpixel(x1,y1,RED);
    p=p+(2*delx);
    }
    else
    {
    x1=x1+1;
    y1=y1+1;
    putpixel(x1,y1,RED);
    p=p+(2*delx)-(2*dely);
    }
    }
    getch();
    closegraph();
     return 0;
    }


