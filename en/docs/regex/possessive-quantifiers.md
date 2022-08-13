---
title: "Possessive Quantifiers"
slug: "possessive-quantifiers"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

NB [Emulating possessive quantifiers](http://stackoverflow.com/q/5537513/256431)

## Basic Use of Possessive Quantifiers
Possessive quantifiers are another class of quantifiers in many regex flavours that allow backtracking to, effectively, be disabled for a given token. This can help improve performance, as well as preventing matches in certain cases.

The class of possessive quantifiers can be distinguished from lazy or greedy quantifiers by the addition of a `+` after the quantifier, as seen below:

| Quantifier   | Greedy | Lazy | Possessive |
| ----------   | ------ | ---- | ---------- |
| Zero or more | `*`    | `*?` | `*+`       |
| One or more  | `+`    | `+?` | `++`       |
| Zero or one  | `?`    | `??` | `?+`       |

Consider, for instance, the two patterns `".*"` and `".*+"`, operating on the string `"abc"d`. In both cases, the `"` at the beginning of the string is matched, but after that the two patterns will have different behaviours and outcomes.

The greedy quantifier will then slurp the rest of the string, `abc"d`. Because this does not match the pattern, it will then backtrack and drop the `d`, leaving the **quantifier** containing `abc"`. Because this still does not match the pattern, the quantifier will drop the `"`, leaving it containing only `abc`. This matches the pattern (as the `"` is matched by a literal, rather than the quantifier), and the regex reports success.

The possessive quantifier will also slurp the rest of the string, but, unlike the greedy quantifier, it will not backtrack. Since its contents, `abc"d`, do not permit the rest of the pattern of the match, the regex will stop and report failure to match.

Because the possessive quantifiers do not do backtracking, they can result in a significant performance increase on long or complex patterns. They can, however, be dangerous (as illustrated above) if one is not aware of how, precisely, quantifiers work internally.

