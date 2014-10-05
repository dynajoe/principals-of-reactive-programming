package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == min(a, b)
  }

  property("hint2") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    h == empty
  }

  property("hint3") = forAll { h : H =>
    def isSorted (h : H) : Boolean = {
      if (isEmpty(h)) true
      else {
        val a = findMin(h)
        val b = deleteMin(h)
        isEmpty(b) || (a <= findMin(b) && isSorted(b))
      }
    }
    isSorted(h)
  }

  property("hint4") = forAll { (a : H, b : H) =>
    val aMin = findMin(a)
    val bMin = findMin(b)

    findMin(meld(a, b)) match {
      case x if x == aMin || x == bMin => true
      case _ => false
    }
  }

  property("meld_order") = forAll { (a : H, b : H) =>
    val m = meld(a, b)
    val m2 = meld(b, a)

    def items (h : H, a: List[A]) : List[A] = {
      if (isEmpty(h)) {
        a
      }
      else {
        val m_ = findMin(h)
        items(deleteMin(h), m_ :: a)
      }
    }

    val mr = items(m, List[A]())
    val mr2 = items(m2, List[A]())

    mr.zip(mr2).forall((a) => a._1 == a._2)
  }

  property("bust")  = forAll { (a: H, b: H) =>
    def heapsEqual (h1 : H, h2: H) : Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val f1 = findMin(h1)
        val f2 = findMin(h2)
        f1 == f2 && (heapsEqual(deleteMin(h1), deleteMin(h2)))
      }
    }
    val m = findMin(a)
    heapsEqual(meld(a, b), meld(deleteMin(a), insert(m, b)))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(empty, genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
