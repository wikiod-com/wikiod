---
title: "Function  wp_trim_words()"
slug: "function--wp_trim_words"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Syntax
- `<?php $trimmed_text = wp_trim_words( $text, $num_words = 55, $more = null ); ?>`



## Parameters
| Parameter  | Details |
| ---------- | ------- |
| `$text`  | (String) (Required) Text that will be shortened or trimmed. |
| `$num_words`  | (Integer) (Required) Number of words to which text will be restricted. |
| `$more`  | (String) (Optional) What to append if $text needs to be trimmed. |

## Trimming post content
This function shortens the text to a specified number of words and returns the shortened text.

    <?php echo wp_trim_words( get_the_content(), 40, '...' ); ?>

In the above example we are passing the post content to the function. It will restrict the length of the content to 40 words and will trim rest of the words.

