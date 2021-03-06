package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("times test 1") {
    new TestTrees {
      assert(times(List()) === List())
    }
  }

  test("times test 2") {
    new TestTrees {
      assert(times(List('a')) === List(('a', 1)))
    }
  }

  test("times test 3") {
    new TestTrees {
      assert(times(List('a', 'a')) === List(('a', 2)))
    }
  }

  test("times test 4") {
    new TestTrees {
      assert(times(List('a', 'b')) === List(('a', 1), ('b', 1)))
    }
  }
  
  test("times test 5") {
    new TestTrees {
      assert(times(List('a', 'b', 'c')) === List(('a', 1), ('b', 1), ('c', 1)))
    }
  }

  test("times test 6") {
    new TestTrees {
      assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
    }
  }

  test("times test 7") {
    new TestTrees {
      assert(times(List('b', 'b', 'a')) === List(('b', 2), ('a', 1)))
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("until test") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(
        until(singleton, combine)(leaflist) ===  
        List(
            Fork(
                Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), 
                Leaf('x',4),
                List('e','t','x'),
                7
            )
        )
    )
  }

  test("decode easy") {
    new TestTrees {
      val code: CodeTree = Fork(Leaf('a',1),Leaf('b',2),List('a','b'),3)

      val secret: List[Bit] = List(0,1,0,1,1,1)
      def decoded: List[Char] = decode(code, secret)
  
      assert(decoded.map(c => c.toString).reduceLeft((x,y) => x + y) === "ababbb")
    }
  }

  test("decode french") {
    new TestTrees {
      def decoded: List[Char] = decode(frenchCode, secret)
  
      assert(decoded.map(c => c.toString).reduceLeft((x,y) => x + y) === "huffmanestcool")
    }
  }

  test("encode easy") {
    new TestTrees {
      val code: CodeTree = Fork(Leaf('a',1),Leaf('b',2),List('a','b'),3)

      val encoded: List[Bit] = encode(code)("ababbb".toList)
  
      assert(encoded === List(0,1,0,1,1,1))
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode & decode french") {
    new TestTrees {
      val decoded: List[Char] = decode(frenchCode, secret)
      
      val encoded = encode(frenchCode)(decoded) 
  
      assert(encoded === secret)
    }
  }
  
  test("convert easy") {
    new TestTrees {
      val code: CodeTree = Fork(Leaf('a',1),Leaf('b',2),List('a','b'),3)

      assert(convert(code) === List(('a', List(0)),('b', List(1))))
    }
  }

  test("convert middle") {
    new TestTrees {
      val code: CodeTree = Fork(Leaf('a',1),Fork(Leaf('b',2),Leaf('c',3),List('b','c'),5),List('a','b','c'),6)

      assert(convert(code) === List(('a',List(0)),('b',List(1,0)),('c',List(1,1))))
    }
  }

  test("quickConvert") {
    new TestTrees {
      val list = "arugula is a vegetable".toList
      assert(quickEncode(frenchCode)(list) === encode(frenchCode)(list))
    }
  }
}
