---
title: "IPython shortcuts, tips and tricks"
slug: "ipython-shortcuts-tips-and-tricks"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

IPython (and the Jupyter Notebook) defines a special meaning for the underscore.  It always contains the the most recent output.  This comes in useful when processing data in multiple steps.  This is demonstrated in the example above.  To clean up the text, it is run through a few regular expressions and then normalized before being split.  Outside of IPython, each result would have to be stored in a new variable or the steps nested.  In IPython, often we are exploring and keeping track of variables or reproducing a long series of nested calls is tedious.  So this is where the underscore appears.  

There is one gotcha.  If you assign a value to the underscore in the global scope, it causes unexpected behavior.  For example:

    address = ('http://example.com', 80)
    (_, port) = address

Here I am only interested in the second element in the tuple, the port number.  So I follow convention and assign the first element to the underscore to indicate it is a throwaway.  However, now the value of the underscore is `http://example.com`.  And if I were to run more code:

    1+4

The expected value of the underscore would be 5.  However, it is not.  The value is still the domain from the tuple.  When you assign to the underscore in the global scope, it not only clobbers the value, but it also stops storing the most recent output.  This is not the case if you assign to the underscore inside a function or loop.

## The special use of the underscore in IPython
    from urllib.request import urlopen
    from collections import Counter
    import re
    
    conn = urlopen('http://textfiles.com/100/dodontae.hum')
    lines = conn.readlines()
    conn.close()
    
    # readlines() returns byte strings
    data = ''.join([line.decode('utf-8') for line in lines]) 
    
    # replace non-letters with a space
    re.sub('[^A-Za-z]', ' ', data) 
    
    # condense successive whitespace into a single space
    # the underscore retrieves the most recent output 
    re.sub('\s+', ' ', _)
    
    # normalize the text by lowercasing and removing leading and trailing whitespace
    _.lower().strip()
    
    # split into words on space
    words = _.split(' ')
    
    from collections import Counter
    word_count = Counter()
    
    for word in words:
        word_count[word[0]] += 1

    word_count.most_common()

