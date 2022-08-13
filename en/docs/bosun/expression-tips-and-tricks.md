---
title: "Expression Tips and Tricks"
slug: "expression-tips-and-tricks"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Avoiding Divide by Zero with NumberSet Operations
In order to avoid a divide by zero with a numberSet (what you get after a reduction like avg()) you can short-circuit the logic:

    $five = min(q("sum:rate{counter,,1}:haproxy.frontend.hrsp{}{status_code=5xx}", "1h", ""))
    $two = avg(q("sum:rate{counter,,1}:haproxy.frontend.hrsp{}{status_code=2xx}", "1h", ""))
    
    $five && $two / $five

If the above were just `$two / $five` then when $five is zero, the result will be `+Inf` which will cause an error when used as warn or crit value in an alert expression.

## Avoiding Divide by Zero in SeriesSet Operations
With series operations, things are dropped from the left side if there is no corresponding timestamp/datapoint in the right side. You can mix this with the dropbool function to avoid divide by zero:

    $five = q("sum:rate{counter,,1}:haproxy.frontend.hrsp{}{status_code=5xx}", "1h", "")
    $two = q("sum:rate{counter,,1}:haproxy.frontend.hrsp{}{status_code=2xx}", "1h", "")
    
    $two / dropbool($five, ($five > 0))

It is possible after dropbool there will be an empty set which would also error. So series operations are recommended for visualization and for alerting it is recommended to use reduction functions earlier in the expression. Alternatively you could wrap the operation in the nv func after reduction: `nv(avg($two / dropbool($five, ($five > 0))), 0)`



