---
title: "Visitors"
slug: "visitors"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

What is the difference between a listener and a visitor?
The difference between listener and visitor mechanisms is listener methods are called by the ANTLR-provided walker object, whereas visitor methods must walk their children with explicit visit calls. Forgetting to invoke visit() on a node’s children means those subtrees don’t get visited. In visitor we have the ability to tree walking while in listener you are only reacting to the tree walker.

## Example
**Grammar Example (Expr.g4)**

    grammar Expr; 
    prog:    (expr NEWLINE)* ;
    expr:    expr ('*'|'/') expr
    
        |    expr ('+'|'-') expr
        |    INT
        |    '(' expr ')'
        ;
    NEWLINE : [\r\n]+ ;
    INT     : [0-9]+ ;

**Generating the visitor**

To generate a Visitor, or to disable a visitor for your grammar you use the following flags:

     -visitor            generate parse tree visitor
     -no-visitor         don't generate parse tree visitor (default)

The commandline/terminal command to build your grammar with a visitor will be formatted as shown below, with respect to flag chosen and possible aliases:

    java - jar antlr-4.5.3-complete.jar Expr.g4 -visitor
    java - jar antlr-4.5.3-complete.jar Expr.g4 -no-visitor

The output will be a parser/lexer with a visitor or no visitor respectively.

**Output**
The output will be **ExprBaseVisitor.java** and **ExprVisitor.java** for this example. These are the relevant java files for you to implement visitor functionality. It is often ideal to create a new class and extend the ExprBaseVisitor to implement new visitor functionality for each method.


    // Generated from Expr.g4 by ANTLR 4.5.3
    import org.antlr.v4.runtime.tree.AbstractParseTreeVisitor;
    
    /**
     * This class provides an empty implementation of {@link ExprVisitor},
     * which can be extended to create a visitor which only needs to handle a subset
     * of the available methods.
     *
     * @param <T> The return type of the visit operation. Use {@link Void} for
     * operations with no return type.
     */
    public class ExprBaseVisitor<T> extends AbstractParseTreeVisitor<T> implements ExprVisitor<T> {
        /**
         * {@inheritDoc}
         *
         * <p>The default implementation returns the result of calling
         * {@link #visitChildren} on {@code ctx}.</p>
         */
        @Override public T visitProg(ExprParser.ProgContext ctx) { return visitChildren(ctx); }
        /**
         * {@inheritDoc}
         *
         * <p>The default implementation returns the result of calling
         * {@link #visitChildren} on {@code ctx}.</p>
         */
        @Override public T visitExpr(ExprParser.ExprContext ctx) { return visitChildren(ctx); }
    }

