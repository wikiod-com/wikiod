---
title: "Trie (Prefix TreeRadix Tree)"
slug: "trie-prefix-treeradix-tree"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Introduction To Trie
Have you ever wondered how the search engines work? How does Google line-up millions of results in front of you in just a few milliseconds? How does a huge database situated thousands of miles away from you find out information you're searching for and send them back to you? The reason behind this is not possible only by using faster internet and super-computers. Some mesmerizing searching algorithms and data-structures work behind it. One of them is [Trie](https://en.wikipedia.org/wiki/Trie).

**Trie**, also called *digital tree* and sometimes *radix tree* or *prefix tree* (as they can be searched by prefixes), is a kind of search tree—an ordered tree data structure that is used to store a dynamic set or associative array where the keys are usually strings. It is one of those data-structures that can be easily implemented. Let's say you have a huge database of millions of words. You can use trie to store these information and the complexity of searching these words depends only on the length of the word that we are searching for. That means it doesn't depend on how big our database is. Isn't that amazing?

Let's assume we have a dictionary with these words:

    algo
    algea
    also
    tom
    to
We want to store this dictionary in memory in such a way that we can easily find out the word that we're looking for. One of the methods would involve sorting the words lexicographically-how the real-life dictionaries store words. Then we can find the word by doing a *binary search*. Another way is using **Prefix Tree** or **Trie**, in short. The word **'Trie'** comes from the word Re**trie**val. Here, **prefix** denotes *the prefix of string* which can be defined like this: All the substrings that can be created starting from the beginning of a string are called prefix. For example: 'c', 'ca', 'cat' all are the prefix of the string 'cat'.

Now let's get back to **Trie**. As the name suggests, we'll create a tree. At first, we have an empty tree with just the root:

[![Root][1]][1]

We'll insert the word **'algo'**. We'll create a new node and name the edge between these two nodes **'a'**. From the new node we'll create another edge named **'l'** and connect it with another node. In this way, we'll create two new edges for **'g'** and **'o'**. Notice that, we're not storing any information in the nodes. For now, we'll only consider creating new edges from these nodes. From now on, let's call an edge named **'x'** - **edge-x**

[![inserting the word algo][2]][2]

Now we want to add the word **'algea'**. We need an **edge-a** from root, which we already have. So we don't need to add new edge. Similarly, we have an edge from **'a'** to **'l'** and **'l'** to **'g'**. That means **'alg'** is already in the **trie**. We'll only add **edge-e** and **edge-a** with it.

[![Inserting the word algea][3]][3]

We'll add the word **'also'**. We have the prefix **'al'** from root. We'll only add **'so'** with it.

[![inserting the word also][4]][4]

Let's add **'tom'**. This time, we create a new edge from root as we don't have any prefix of tom created before.

[![Inserting the word tom][5]][5]

Now how should we add **'to'**? **'to'** is completely a prefix of **'tom'**, so we don't need to add any edge. What we can do is, we can put end-marks in some nodes. We'll put end marks in those nodes where at least one word is completed. Green circle denotes the end-mark. The trie will look like:

[![Inserting the word to][6]][6]

You can easily understand why we added the end-marks. We can determine the words stored in trie. The characters will be on the edges and nodes will contain the end-marks.

Now you might ask, what's the purpose of storing words like this? Let's say, you are asked to find the word **'alice'** in the dictionary. You'll traverse the trie. You'll check if there is an **edge-a** from root. Then check from **'a'**, if there's an **edge-l**. After that, you won't find any **edge-i**, so you can come to the conclusion that, the word **alice** doesn't exist in the dictionary.

If you're asked to find the word **'alg'** in the dictionary, you'll traverse **root->a**, **a->l**, **l->g**, but you won't find a green node at the end. So the word doesn't exist in the dictionary. If you search for **'tom'**, you'll end up in a green node, which means the word exists in the dictionary.

**Complexity:**

The maximum amount of steps needed to search for a word in a **trie** is the length of the word that we're looking for. The complexity is **O(length)**. The complexity for insertion is same. The amount of memory needed to implement **trie** depends on the implementation. We'll see an implementation in another example where we can store **10<sup>6</sup>** characters (not words, letters) in a **trie**.

**Use of Trie:**

 - To insert, delete and search for a word in dictionary.
 - To find out if a string is a prefix of another string.
 - To find out how many strings has a common prefix.
 - Suggestion of contact names in our phones depending on the prefix we enter.
 - Finding out 'Longest Common Substring' of two strings.
 - Finding out the length of 'Common Prefix' for two words using 'Longest Common Ancestor'

  [1]: http://i.stack.imgur.com/dIrLO.png
  [2]: http://i.stack.imgur.com/WCa2D.png
  [3]: http://i.stack.imgur.com/Zkh7E.png
  [4]: http://i.stack.imgur.com/rDxP5.png
  [5]: http://i.stack.imgur.com/G5Dui.png
  [6]: http://i.stack.imgur.com/PNY3a.png

## Implementation of Trie
*Before reading this example, it is highly recommended that you read [Introduction to Trie](https://www.wikiod.com/data-structures/trie-prefix-treeradix-tree#Introduction To Trie) first.*

One of the easiest ways of implementing **Trie** is using linked list.

**Node:**

The nodes will consist of:
 1. Variable for End-Mark.
 2. Pointer Array to the next Node.

The End-Mark variable will simply denote whether it is an end-point or not. The pointer array will denote all the possible edges that can be created. For English alphabets, the size of the array will be 26, as maximum 26 edges can be created from a single node. At the very beginning each value of pointer array will be NULL and the end-mark variable will be false.

    Node:
    Boolean Endmark
    Node *next[26]
    Node()
        endmark = false
        for i from 0 to 25
            next[i] := NULL
        endfor
    endNode

Every element in **next[]** array points to another node. **next[0]** points to the node sharing **edge-a**, **next[1]** points to the node sharing **edge-b** and so on. We have defined the constructor of Node to initialize Endmark as false and put NULL in all the values of **next[]** pointer array.

To create **Trie**, at first we'll need to instantiate **root**. Then from the **root**, we'll create other nodes to store information.

**Insertion:**

    Procedure Insert(S, root):    // s is the string that needs to be inserted in our dictionary
    curr := root
    for i from 1 to S.length
        id := S[i] - 'a'
        if curr -> next[id] == NULL
            curr -> next[id] = Node()
        curr := curr -> next[id]
    endfor
    curr -> endmark = true

Here we are working with **a-z**. So to convert their ASCII values to **0-25**, we subtract the ASCII value of **'a'** from them. We will check if current node (curr) has an edge for the character at hand. If it does, we move to the next node using that edge, if it doesn't we create a new node. At the end, we change the endmark to true.

**Searching:**

    Procedure Search(S, root)      // S is the string we are searching
    curr := root
    for i from 1 to S.length
        id := S[i] - 'a'
        if curr -> next[id] == NULL
            Return false
        curr := curr -> id
    Return curr -> endmark
The process is similar to insert. At the end, we return the **endmark**. So if the     **endmark** is true, that means the word exists in the dictionary. If it's false, then the word doesn't exist in the dictionary.

This was our main implementation. Now we can insert any word in **trie** and search for it.

**Deletion:**

Sometimes we will need to erase the words that will no longer be used to save memory. For this purpose, we need to delete the unused words:

    Procedure Delete(curr):       //curr represents the current node to be deleted
    for i from 0 to 25
        if curr -> next[i] is not equal to NULL
            delete(curr -> next[i])
        delete(curr)               //free the memory the pointer is pointing to
This function goes to all the child nodes of **curr**, deletes them and then **curr** is deleted to free the memory. If we call **delete(root)**, it will delete the whole **Trie**.

**Trie** can be implemented using *Arrays* too.

