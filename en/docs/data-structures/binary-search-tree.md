---
title: "Binary Search Tree"
slug: "binary-search-tree"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Creating a Node in BST
The Binary Search Tree (BST) is a hierarchical data structure with a single pointer to the root node. 

The Node in the BST generally contains "items" (such as numbers or names) for fast look up. Each node has at-most two children (left and right). Every node is organized by some key data field. For every node in BST its key is greater than left child's key and less than right child's key

A typical structure of node (which stores an integer) would be

    struct bst_node {
        int item; 
        bst_node* left;
        bst_node* right;
    }; 

There will be only one root node of BST. The root node can be created by

    bst_node* root = NULL;
    root = (bst_node*) malloc(sizeof(bst_node));
  
To set item key of root to 10.

    root->item = 10;


## inserting a node into binary search tree
    struct tree{
        int a;
        tree* right;
        tree* left;
    };
    tree* root=NULL;
    void insert(tree*& in, int b){
            if(in){
                if(in->a<b)
                        insert(in->right,b);
                else if(in->a>b)
                        insert(in->left,b);
                else
                    cout<<"the value is already in the tree."<<endl;
            }else{
                tree* temp = new tree;
                temp->a=b;
                temp->right=NULL;
                temp->left=NULL;
                in=temp;
            }
    }

