---
title: "Typeclass derivation"
slug: "typeclass-derivation"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## ProductTypeClass
    trait Show[T] {
      def show(t: T): String
    }
    
    object Show extends ProductTypeClassCompanion[Show] {
    
      def apply[T](implicit T: Show[T]) = T
    
      def from[T](f: T => String): Show[T] = new Show[T] {
        def show(t: T): String = f(t)
      }
    
      implicit val string = from[String](_.reverse)
      implicit val int = from[Int](Integer.toHexString)
    
      object typeClass extends ProductTypeClass[Show] {
        override def product[H, T <: HList](ch: Show[H], ct: Show[T]): Show[::[H, T]] = from(ht =>
          ch.show(ht.head) + " " + ct.show(ht.tail)
        )
    
        override def emptyProduct: Show[HNil] = from(_ => "")
    
        override def project[F, G](instance: => Show[G], to: (F) => G, from: (G) => F): Show[F] =
          Show.from((f: F) => instance.show(to(f)))
      }
    }
    
    case class Example(name: String, value: Int)

    Show[Example].show(Example("Me", 12))  // returns "eM c"

