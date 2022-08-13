---
title: "Common functors as the base of cofree comonads"
slug: "common-functors-as-the-base-of-cofree-comonads"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Cofree Empty ~~ Empty
Given

    data Empty a

we have

    data Cofree Empty a
       --  = a :< ...  not possible!

## Cofree (Const c) ~~ Writer c
Given

    data Const c a = Const c

we have

    data Cofree (Const c) a
         = a :< Const c

which is isomorphic to

    data Writer c a = Writer c a

## Cofree Identity ~~ Stream
Given

    data Identity a = Identity a

we have

    data Cofree Identity a
         = a :< Identity (Cofree Identity a)

which is isomorphic to

    data Stream a = Stream a (Stream a)

## Cofree Maybe ~~ NonEmpty
Given

    data Maybe a = Just a
                 | Nothing

we have

    data Cofree Maybe a
         = a :< Just (Cofree Maybe a)
         | a :< Nothing

which is isomorphic to

    data NonEmpty a
         = NECons a (NonEmpty a)
         | NESingle a

## Cofree (Writer w) ~~ WriterT w Stream
Given

    data Writer w a = Writer w a

we have

    data Cofree (Writer w) a
         = a :< (w, Cofree (Writer w) a)

which is equivalent to

    data Stream (w,a)
         = Stream (w,a) (Stream (w,a))

which can properly be written as `WriterT w Stream` with

    data WriterT w m a = WriterT (m (w,a))

## Cofree (Either e) ~~ NonEmptyT (Writer e)
Given

    data Either e a = Left e
                    | Right a

we have

    data Cofree (Either e) a
         = a :< Left e
         | a :< Right (Cofree (Either e) a)

which is isomorphic to

    data Hospitable e a
         = Sorry_AllIHaveIsThis_Here'sWhy a e
         | EatThis a (Hospitable e a)

or, if you promise to only evaluate the log after the complete result, `NonEmptyT (Writer e) a` with

    data NonEmptyT (Writer e) a = NonEmptyT (e,a,[a])

## Cofree (Reader x) ~~ Moore x
Given

    data Reader x a = Reader (x -> a)

we have

    data Cofree (Reader x) a
         = a :< (x -> Cofree (Reader x) a)

which is isomorphic to

    data Plant x a
         = Plant a (x -> Plant x a)

aka [Moore machine](http://hackage.haskell.org/package/machines-0.6.1/docs/Data-Machine-Moore.html#t:Moore).

