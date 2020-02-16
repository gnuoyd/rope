//
//	RopeTests.swift
//	RopeTests
//
//	Created by David Young on 19 Aug 19.
//	Copyright © 2019 White Coral Islands. All rights reserved.
//

import XCTest
@testable import Rope

typealias NSS = Node<Substring>
typealias RSS = Rope<Substring>

infix operator ⨯: MultiplicationPrecedence

func ⨯<L, R, Lseq : Sequence, Rseq : Sequence>(_ l: Lseq, _ r: Rseq) -> LazySequence<FlattenSequence<LazyMapSequence<Lseq, LazyMapSequence<Rseq, (L, R)>>>>  where Lseq.Element == L, Rseq.Element == R {
	return l.lazy.flatMap({ lelt in r.lazy.map({ relt in (lelt, relt) })})
}

class IndexOrder: XCTestCase {
	func testComparingIndicesSequentially() {
		let rope = Rope<Substring>(content: "pqrstuvwxyz")
		var previous: Rope<Substring>.Index? = nil
		for current in rope.indices {
			guard let p = previous else {
				previous = current
				continue
			}
			XCTAssert(p < current)
			XCTAssert(!(current < p))
			XCTAssert(!(current == p))
		}
	}

	func testComparingIndicesPairwise() {
		let rope = Rope<Substring>(content: "pqrstuvwxyz")
		let indices = rope.indices.enumerated()
		for (l, r) in indices ⨯ indices {
			XCTAssert((l.offset < r.offset) == (l.element < r.element))
		}
	}
}

class ContainerElementLookupUsingRopeIndices: XCTestCase {
	let h = Handle()
	let nodel: NSS = Node(content: "abc")
	let noder: NSS = Node(content: "def")
	var _expectations: [NSS]? = nil
	var _r: RSS? = nil
	var expectations: [NSS] {
		get {
			if let olde = _expectations {
				return olde
			}
			let newe: [NSS] = [.empty,
			    Node(handle: h, node: Node(content: "a")),
			    Node(handle: h, node: Node(content: "b")),
			    Node(handle: h, node: Node(content: "c")),
			    Node(handle: h, node: .empty),
			    Node(content: "d"),
			    Node(content: "e"),
			    Node(content: "f")]
			_expectations = newe
			return newe
		}
	}
	var r: RSS {
		get {
			if let oldr = _r {
				return oldr
			}
			let newr: RSS = Rope()
			newr.node = Node(left: Node(handle: h, node: nodel),
			                 right: noder)
			_r = newr
			return newr
		}
	}
	func testElementsCount() {
		XCTAssert(r.count == expectations.count)
	}
	func testIterateElements() {
		for (found, expected) in zip(r, expectations) {
			XCTAssert(found == expected)
		}
	}
	func testIndicesCount() {
		XCTAssert(r.indices.count == expectations.count)
	}
	func testLookupByIndices() {
		for (idx, expected) in zip(r.indices, expectations) {
			let found = r[idx]
			XCTAssert(found == expected)
		}
	}
	func testStepIndices() {
		var idx = r.startIndex
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		print(idx)
	}
	func testEndIndices() {
		let idx = r.endIndex
		XCTAssertThrowsError(try r.element(at: idx))
	}
}

class BasicElementLookupUsingRopeIndices: XCTestCase {
	func testStartIndices() {
		let rope1 = Rope<Substring>(content: "abc")
		let rope2 = Rope<Substring>(content: "def")
		let idx1 = rope1.startIndex
		let idx2 = rope2.startIndex
		XCTAssert(rope1[idx1].content == "a")
		XCTAssert(rope2[idx2].content == "d")
	}
	func testSecondIndices() {
		let rope1 = Rope<Substring>(content: "abc")
		let rope2 = Rope<Substring>(content: "def")
		let idx1 = rope1.index(after: rope1.startIndex)
		let idx2 = rope2.index(after: rope2.startIndex)
		XCTAssert(rope1[idx1].content == "b")
		XCTAssert(rope2[idx2].content == "e")
	}
	func testThirdIndices() {
		let rope1 = Rope<Substring>(content: "abc")
		let rope2 = Rope<Substring>(content: "def")
		let idx1 = rope1.index(after: rope1.index(after: rope1.startIndex))
		let idx2 = rope2.index(after: rope2.index(after: rope2.startIndex))
		XCTAssert(rope1[idx1].content == "c")
		XCTAssert(rope2[idx2].content == "f")
	}
	func testEndIndices() {
		let rope1 = Rope<Substring>(content: "abc")
		let rope2 = Rope<Substring>(content: "def")
		let idx1 = rope1.endIndex
		let idx2 = rope2.endIndex
		XCTAssertThrowsError(try rope1.element(at: idx1))
		XCTAssertThrowsError(try rope2.element(at: idx2))
	}
}

class CompareDisparateRopeIndices: XCTestCase {
	func testStartIndices() {
		let rope1 = Rope<Substring>(content: "abc")
		let rope2 = Rope<Substring>(content: "def")
		let idx1 = rope1.startIndex
		let idx2 = rope2.startIndex
		XCTAssertThrowsError(try idx1.isLessThan(idx2))
		XCTAssertThrowsError(try idx1.equals(idx2))
	}
	func testSecondIndices() {
		let rope1 = Rope<Substring>(content: "abc")
		let rope2 = Rope<Substring>(content: "def")
		let idx1 = rope1.index(after: rope1.startIndex)
		let idx2 = rope2.index(after: rope2.startIndex)
		XCTAssertThrowsError(try idx1.isLessThan(idx2))
		XCTAssertThrowsError(try idx1.equals(idx2))
	}
	func testEndIndices() {
		let rope1 = Rope<Substring>(content: "abc")
		let rope2 = Rope<Substring>(content: "def")
		let idx1 = rope1.endIndex
		let idx2 = rope2.endIndex
		XCTAssertThrowsError(try idx1.isLessThan(idx2))
		XCTAssertThrowsError(try idx1.equals(idx2))
	}
}

class FibonacciTests : XCTestCase {
	func testFibonacciByIndex() {
		XCTAssert([0, 1, 2, 3, 4, 5].map({ i in fibonacci(index: i)}) == [0, 1, 1, 2, 3, 5])
	}
	
	func testFibonacciGenerator() {
		// Produce the Fibonacci sequence, 0th through 5th element.
		let arr = Fibonacci(through: 5).reduce([], { (arr: [UInt], elt: UInt) -> [UInt] in arr + [elt]})

		XCTAssert(arr == [0, 1, 1, 2, 3, 5])
	}
}

class HandleHolding : XCTestCase {

/*
	override func setUp() {
		// Put setup code here. This method is called before the invocation of each test method in the class.
	}

	override func tearDown() {
		// Put teardown code here. This method is called after the invocation of each test method in the class.
	}
*/

	/* Test the expectation that if the only remaining reference to an
	 * object is the reference held by a Weak struct,
	 * w, then trying to retrieve the object with w.get() yields nil.
	 */
	func testReleaseHandle() {
		var h = Handle()
		let w = Weak(h)
		h = Handle()
		XCTAssert(w.get() == nil)
	}

	/* Test the expectation that if the only remaining references to an
	 * object are the references held by Weak structs,
	 * w and x, then trying to retrieve the object with w.get() and
	 * x.get() yields nil.
	 */
	func testReleaseTwoHandles() {
		var h = Handle()
		let w = Weak(h)
		let x = Weak(h)
		h = Handle()
		XCTAssert(w.get() == nil)
		XCTAssert(x.get() == nil)
	}

	/* Test the expectation that if there is a second reference to an
	 * object, o, that is also held by a Weak struct, w, then the
	 * object retrieved by w.get() is o.
	 */
	func testHoldHandle() {
		let h = Handle()
		let w = Weak(h)
		XCTAssert(w.get() == h)
	}
	
	func testStepAndHoldIndex() {
		let first = Node<Substring>(content: "abc")
		let handle = Handle()
		guard case .step(let second) = first.afterStepInsertingIndex(handle) else {
			XCTFail("afterStepInserting failed")
			return
		}
		XCTAssert(second.leaves.map({ (x: Node<Substring>) -> Bool in if case .index(let w) = x { return w.get() == handle } else {return false } })[1])
	}
	
	func testStepAndReleaseIndex() {
		let first = Node<Substring>(content: "abc")
		var handle = Handle()
		guard case .step(let second) = first.afterStepInsertingIndex(handle) else {
			XCTFail("afterStepInserting failed")
			return
		}
		handle = Handle()
		XCTAssert(!second.leaves.map({ (x: Node<Substring>) -> Bool in if case .index(let w) = x { return w.get() == handle } else {return false } })[1])
	}

	static func isIndex(_ n: Node<Substring>) -> Bool {
		if case .index(_) = n {
			return true
		}
		return false
	}

	static func isNilIndex(_ n: Node<Substring>) -> Bool {
		if case .index(let w) = n {
			return w.get() == nil
		}
		return false
	}
	
	func testCleanedHoldingIndices() {
		let emptyRope = Rope<Substring>(content: "abcdefghijkl")
		var indices: [Rope<Substring>.Index]? = []
		
		for i in emptyRope.indices {
			indices?.append(i)
		}
		print(emptyRope.node)
		XCTAssert(emptyRope.node.cleaned()?.leaves.filter(HandleHolding.isIndex).count == 11)
	}

	func testCleanedReleasingIndices() {
		let emptyRope = Rope<Substring>(content: "abcdefghijkl")
		var indices: [Rope<Substring>.Index]? = []

		for i in emptyRope.indices {
			indices?.append(i)
		}
		print(emptyRope.node)
		indices = nil
		XCTAssert(emptyRope.node.cleaned()?.leaves.filter(HandleHolding.isIndex).count == 0)
	}
	
	func testReleasingIndices() {
		let rope = Rope<Substring>(content: "abcdefghijkl")
		var indices: [Rope<Substring>.Index]? = []
		
		for i in rope.indices {
			indices?.append(i)
		}

		indices = nil
		print(rope.node.leaves)
		print(rope.node.leaves.filter(HandleHolding.isNilIndex))

		XCTAssert(rope.node.leaves.filter(HandleHolding.isNilIndex).count == 11)
	}

	func testPerformanceExample() {
		// This is an example of a performance test case.
		self.measure {
			// Put the code you want to measure the time of here.
		}
	}
}

class NodeSubropes : XCTestCase {
	let n: NSS = Node(left: Node(content: "abc"), right: Node(left: Node(content: "defgh"), right: Node(content: "ijkl")))

	func testFullContent() {
		XCTAssert(n.content == "abcdefghijkl")
	}
	
	func testLeadingSubnode() {
		XCTAssert(n.subrope(from: NodeIndex(utf16Offset: 0), to: NodeIndex(utf16Offset: 3)).content == "abc")
	}

	func testCrossingFirstTwoSubnodes() {
		XCTAssert(n.subrope(from: NodeIndex(utf16Offset: 0), to: NodeIndex(utf16Offset: 5)).content == "abcde")
	}

	func testSecondSubnode() {
		XCTAssert(n.subrope(from: NodeIndex(utf16Offset: 3), to: NodeIndex(utf16Offset: 8)).content == "defgh")
	}

	func testTrailingTwoSubnodes() {
		XCTAssert(n.subrope(from: NodeIndex(utf16Offset: 3), to: NodeIndex(utf16Offset: 12)).content == "defghijkl")
	}

	func testCrossingLastTwoSubnodes() {
		XCTAssert(n.subrope(from: NodeIndex(utf16Offset: 4), to: NodeIndex(utf16Offset: 9)).content == "efghi")
	}
}

class RopeTextStorage : XCTestCase {
}
