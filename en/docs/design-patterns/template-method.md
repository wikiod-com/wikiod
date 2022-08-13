---
title: "Template Method"
slug: "template-method"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Template method implementation in java
Template method pattern is a behavioral design pattern that defines the program skeleton of an algorithm in an operation, defering some steps to subclasses.

**Structure:**

[![enter image description here][1]][1]

**Key notes:**

1. Template method uses Inheritance
2. The Template method implemented by the base class should not be overridden. In this way, the structure of the algorithm is controlled by the super class, and the details are implemented in the sub classes

**Code example:**

    import java.util.List;
    
    class GameRule{
    
    }
    class GameInfo{
        String gameName;
        List<String> players;
        List<GameRule> rules;
    }    
    
    abstract class Game{
        protected GameInfo info;
        public Game(GameInfo info){
            this.info = info;
        }
        public abstract void createGame();
        public abstract void makeMoves();
        public abstract void applyRules();
        
        /* playGame is template method. This algorithm skeleton can't be changed by sub-classes. sub-class can change
           the behaviour only of steps like createGame() etc. */
           
        public void playGame(){
            createGame();
            makeMoves();
            applyRules();
            closeGame();
        }
        protected void closeGame(){
            System.out.println("Close game:"+this.getClass().getName());
            System.out.println("--------------------");
        }
    }
    class Chess extends Game{
        public Chess(GameInfo info){
            super(info);
        }
        public void createGame(){
            // Use GameInfo and create Game
            System.out.println("Creating Chess game");
        }
        public void makeMoves(){
            System.out.println("Make Chess moves");
        }
        public void applyRules(){
            System.out.println("Apply Chess rules");
        }
    }
    class Checkers extends Game{
        public Checkers(GameInfo info){
            super(info);
        }
        public void createGame(){
            // Use GameInfo and create Game
            System.out.println("Creating Checkers game");
        }
        public void makeMoves(){
            System.out.println("Make Checkers moves");
        }
        public void applyRules(){
            System.out.println("Apply Checkers rules");
        }
        
    }
    class Ludo extends Game{
        public Ludo(GameInfo info){
            super(info);
        }
        public void createGame(){
            // Use GameInfo and create Game
            System.out.println("Creating Ludo game");
        }
        public void makeMoves(){
            System.out.println("Make Ludo moves");
        }
        public void applyRules(){
            System.out.println("Apply Ludo rules");
        }
    }
    
    public class TemplateMethodPattern{
        public static void main(String args[]){
            System.out.println("--------------------");
        
            Game game = new Chess(new GameInfo());
            game.playGame();
            
            game = new Ludo(new GameInfo());
            game.playGame();
            
            game = new Checkers(new GameInfo());
            game.playGame();
        }
    }

Explanation:

1. `Game` is an `abstract` super class, which defines a template method : `playGame()`
2. Skeleton of `playGame()` is defined in base class: `Game`
3. Sub-classes like `Chess, Ludo` and `Checkers` can't change the skeleton of `playGame()`. But they can modify the behaviour of some steps like 
         
       createGame();
       makeMoves();
       applyRules();
  
output:

    --------------------
    Creating Chess game
    Make Chess moves
    Apply Chess rules
    Close game:Chess
    --------------------
    Creating Ludo game
    Make Ludo moves
    Apply Ludo rules
    Close game:Ludo
    --------------------
    Creating Checkers game
    Make Checkers moves
    Apply Checkers rules
    Close game:Checkers
    --------------------


  [1]: https://i.stack.imgur.com/TCAsQ.png

