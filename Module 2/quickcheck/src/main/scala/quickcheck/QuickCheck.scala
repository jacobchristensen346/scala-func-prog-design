package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for
      i <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    yield
      insert(i, h)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H) =>
    val k = if isEmpty(h) then insert(0, h) else h
    val m = findMin(k)
    findMin(deleteMin(insert(m, k))) == m
  }

  property("gen3") = forAll { (h: H) =>
    val melded = meld(h, h)
    val minMelded = if isEmpty(melded) then 0 else findMin(melded)
    val minH = if isEmpty(h) then 0 else findMin(h) 
    minMelded == minH
  }

  property("gen4") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    if -m.abs > Int.MinValue + 1 then {
      val smaller = insert(m - 1, h)
      val larger = insert(m + 1, h)
      val melded = meld(larger, smaller)
      findMin(melded) == m - 1
    } else true
  }

  property("gen5") = forAll { (h: H) =>
    if isEmpty(h) then true
    else {
      val m = findMin(h)
      if -m.abs > Int.MinValue + 1 then {
        val nH = insert(m + 1, h)
        if findMin(nH) != m then {
          val lH = deleteMin(nH)
          val min = findMin(lH)
          min == m + 1
        } else true
      } else true
    }
  }

  property("gen6") = forAll { (h: H) =>
    if isEmpty(h) then {
      val newH = insert(1, insert(0, h))
      findMin(newH) == 0
    } else true
  }

  property("gen7") = forAll { (h: H) =>
    if isEmpty(h) then {
      isEmpty(deleteMin(insert(1, h)))
    } else true
  }

  property("gen8") = forAll { (nums: List[Int]) =>
    var heap = nums.foldLeft(empty)((h, x) => insert(x, h))
    def drain(h: H, acc: List[Int]): List[Int] = 
      if (isEmpty(h)) acc.reverse
      else {
        val min = findMin(h)
        drain(deleteMin(h), min :: acc)
      }
    val result = drain(heap, Nil)
    result == nums.sorted
  }

  property("gen9") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) then true
    else
      findMin(meld(h1, h2)) == math.min(findMin(h1), findMin(h2))
  }

