---
title: "Geometry"
slug: "geometry"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Calculate Angle from Three Points
Lets first understand the problem, consider this figure-

[![angle][1]][1]

We want to calculate *ϴ*, where we know *A*, *B* & *O*.

Now, if we want to get *ϴ*, we need to find out *α* and *β* first. For any straight line, we know- 

    y = m * x + c

Let- *A = (ax, ay)*, *B = (bx, by)*, and *O = (ox, oy)*. So for the line *OA*-

    oy = m1 * ox + c   ⇒ c = oy - m1 * ox   ...(eqn-1)
    
    ay = m1 * ax + c   ⇒ ay = m1 * ax + oy - m1 * ox   [from eqn-1]
                       ⇒ ay = m1 * ax + oy - m1 * ox
                       ⇒ m1 = (ay - oy) / (ax - ox)
                       ⇒ tan α = (ay - oy) / (ax - ox)   [m = slope = tan ϴ]   ...(eqn-2)

In the same way, for line *OB*-

    tan β = (by - oy) / (bx - ox)   ...(eqn-3)

Now, we need `ϴ = β - α`. In trigonometry we have a formula-

    tan (β-α) = (tan β + tan α) / (1 - tan β * tan α)   ...(eqn-4)

After replacing the value of `tan α` (from eqn-2) and `tan b` (from eqn-3) in eqn-4, and applying simplification we get-

    tan (β-α) = ( (ax-ox)*(by-oy)+(ay-oy)*(bx-ox) ) / ( (ax-ox)*(bx-ox)-(ay-oy)*(by-oy) )

So,

    ϴ = β-α = tan^(-1) ( ((ax-ox)*(by-oy)+(ay-oy)*(bx-ox)) / ((ax-ox)*(bx-ox)-(ay-oy)*(by-oy)) )

That is it! 

Now, take the following figure-

[![angle][2]][2]

Following C# or, Java method implements above theory-

        double calculateAngle(double P1X, double P1Y, double P2X, double P2Y,
                double P3X, double P3Y){
     
            double numerator = P2Y*(P1X-P3X) + P1Y*(P3X-P2X) + P3Y*(P2X-P1X);
            double denominator = (P2X-P1X)*(P1X-P3X) + (P2Y-P1Y)*(P1Y-P3Y);
            double ratio = numerator/denominator;

            double angleRad = Math.Atan(ratio);
            double angleDeg = (angleRad*180)/Math.PI;

            if(angleDeg<0){
                angleDeg = 180+angleDeg;
            }

            return angleDeg;
        }


  [1]: http://i.stack.imgur.com/Dcqwn.png
  [2]: http://i.stack.imgur.com/ZLwJe.png

