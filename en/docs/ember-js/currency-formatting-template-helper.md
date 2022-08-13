---
title: "Currency formatting template helper"
slug: "currency-formatting-template-helper"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

More details available in [Ember guides][1], where this example was taken from.

Compatible with Ember 2.2.0+ (2.11.0 was the latest at the time of writing)

  [1]: https://guides.emberjs.com/v2.11.0/templates/writing-helpers/

## Creating a new helper
Use Ember CLI to generate a new helper in your app:

`ember generate helper format-currency`

Then edit `helpers/format-currency.js` to contain the following:
```
import Ember from 'ember';

export function formatCurrency([value, ...rest]) {
  const dollars = Math.floor(value / 100);
  const cents = value % 100;
  const sign = '$';

  if (cents.toString().length === 1) { cents = '0' + cents; }
  return `${sign}${dollars}.${cents}`;
}

export default Ember.Helper.helper(formatCurrency);
```

Now you can use `{{format-currency model.someNumericValue}}` in templates.

---
A unit test for the new helper is automatically created in `tests/unit/helpers/`

