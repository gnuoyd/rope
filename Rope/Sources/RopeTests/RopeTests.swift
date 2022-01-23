//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
import XCTest
@testable import Rope

typealias RSS = Rope<Substring>
typealias NSS = RSS.Node
typealias RWZC = RSS.ZoneController
typealias ROZC = RSS.ReadonlyZoneController
typealias Offset = NSS.Offset

infix operator ⨯: MultiplicationPrecedence

func ⨯<L, R, Lseq : Sequence, Rseq : Sequence>(_ l: Lseq, _ r: Rseq)
    -> LazySequence<
        FlattenSequence<LazyMapSequence<Lseq, LazyMapSequence<Rseq, (L, R)>>>>
    where Lseq.Element == L, Rseq.Element == R {
	return l.lazy.flatMap { lelt in
		r.lazy.map { relt in (lelt, relt) }
	}
}

extension Range where Bound : Comparable {
	func contains(_ other: Range<Bound>) -> Bool {
		return self.lowerBound <= other.lowerBound &&
		       other.upperBound <= self.upperBound
	}
	func intersects(_ other: Range<Bound>) -> Bool {
		/* self |-----|
		 * other      |-----|
		 *
		 * self        |-----|
		 * other |-----|
		 */
		return !(other.upperBound <= self.lowerBound ||
		         self.upperBound <= other.lowerBound)
	}
}

extension Range where Bound : Comparable {
	func strictlyOverlaps(_ other: Range<Bound>) -> Bool {
		return other.clamped(to: self) == other ||
		       self.clamped(to: other) == self
	}
}

extension Rope {
	class ReadonlyZoneController : ZoneController {
		override func transformingAttributes(
		    after lowerBound: Label, upTo upperBound: Label,
		    in content: Rope.Node,
		    andBoundaries boundaries: BoundarySet = .neither,
		    with fn: (Attributes) -> Attributes) throws -> Rope.Node {
			throw Rope.Node.NodeError.readonlyZone
		}
		override func replacing(
		    after lowerBound: Label, upTo upperBound: Label,
		    in content: Rope.Node,
		    with replacement: Rope.Node,
		    undoList: ChangeList<Rope.Node>?) throws -> Rope.Node {
			throw Rope.Node.NodeError.readonlyZone
		}
		override func setController(_ ctlr: ZoneController,
		    after lowerBound: Label, upTo upperBound: Label,
		    in content: Rope.Node,
		    undoList: ChangeList<Rope.Node>?) throws -> Rope.Node {
			throw Rope.Node.NodeError.readonlyZone
		}
	}
}

class IndexOrderByIndex: XCTestCase {
	func testComparingIndicesSequentially() {
		let labels = [Label(), Label(), Label()]
		let rope: RSS = Rope(with:
		    labels.map { NSS.index(label: $0) }
		          .reduce(.empty) { (tree, next) in
			                    .nodes(tree, next) } )
		var previous: Label? = nil
		for label in labels {
			guard let p = previous else {
				previous = label
				continue
			}
			XCTAssert(try rope.node.label(p, precedes: label,
			    by: .jot))
			XCTAssert(try !rope.node.label(label, precedes: p,
			    by: .jot))
		}
	}
	func testComparingIndicesPairwise() {
		let labels = [Label(), Label(), Label()]
		let rope: RSS = Rope(with:
		    labels.map { NSS.index(label: $0) }
		          .reduce(.empty) { (tree, next) in
			                    .nodes(tree, next) } )
		let indices = rope.indices.enumerated()
		for (l, r) in indices ⨯ indices {
			XCTAssert(try (l.offset < r.offset) ==
			          rope.node.label(l.element.label,
				                   precedes: r.element.label,
						   by: .jot))
		}
	}
	func testFollow() {
		let labels = [Label(), Label(), Label()]
		let rope: RSS = Rope(with:
		    labels.map { NSS.index(label: $0) }
		          .reduce(.empty) { (tree, next) in
			                    .nodes(tree, next) } )
		for label in labels.dropLast() {
			XCTAssert(try rope.node.any(.jot, follows: label))
		}
		guard let last = labels.last else {
			XCTFail("No last array element.")
			return
		}
		XCTAssert(try !rope.node.any(.jot, follows: last))
	}
	func testPrecede() {
		let labels = [Label(), Label(), Label()]
		let rope: RSS = Rope(with:
		    labels.map { NSS.index(label: $0) }
		          .reduce(.empty) { (tree, next) in
			                    .nodes(tree, next) } )
		for label in labels.dropFirst() {
			XCTAssert(try rope.node.any(.jot, precedes: label))
		}
		guard let first = labels.first else {
			XCTFail("No first array element.")
			return
		}
		XCTAssert(try !rope.node.any(.jot, precedes: first))
	}
}

class IndexOrderByStep: XCTestCase {
	func testComparingIndicesSequentially() {
		let rope: RSS = Rope(content: "pqrstuvwxyz")
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
		let rope: RSS = Rope(content: "pqrstuvwxyz")
		let indices = rope.indices.enumerated()
		for (l, r) in indices ⨯ indices {
			XCTAssert((l.offset < r.offset) ==
			          (l.element < r.element))
		}
	}
}

class NestedZoneBase : XCTestCase {
	let c = [RWZC(), RWZC(), RWZC()]
	// (abc(def(ghi)))
	// 000000000000000
	//     1111111111
	//         22222
	lazy var rope: RSS = Rope(with:
		    .zone(under: c[0],
		            .text("abc"),
			    .zone(under: c[1],
			            .text("def"),
				    .zone(under: c[2],
				            .text("ghi")))))
}

class StepContent : NestedZoneBase {
	static func testSubsteps(_ rope: RSS, _ start: Int, _ end: Int) {
		var steps = Array<RSS.Content.Unit>(repeating: "x".utf16.first!, count: 15)
		let units = RSS.BoundaryUnits(open: "⟨".utf16.only!, close: "⟩".utf16.only!)
		steps.withContiguousMutableStorageIfAvailable {
		    (_ buf: inout UnsafeMutableBufferPointer<RSS.Content.Unit>) in
			guard var base = buf.baseAddress else {
				XCTFail("could not get base address of steps")
				return
			}
			rope.node.extract(from: start, upTo: end, on: \.steps,
			    filling: &base, defaults: units)
		}
		let expected = "⟨abc⟨def⟨ghi⟩⟩⟩".utf16.map { $0 }[start..<end]
		XCTAssert(steps[..<(end - start)] == expected,
		    "\(steps[..<(end - start)]) != \(expected), start \(start), end \(end)")
	}
	func testAllStepRanges() {
		for (start, end) in (0...15) ⨯ (0...15) {
			if start > end {
				continue
			}
			StepContent.testSubsteps(rope, start, end)
		}
	}
}

class BoundaryIndexComparisons : XCTestCase {
	let c = [RWZC(), RWZC(), RWZC()]
	let l = Array<Label>((0..<3).map { _ in Label() })
	lazy var empty: RSS = Rope(with: .empty)
	lazy var allIndices: RSS = Rope(with:
	    .index(label: l[0]), .index(label: l[1]), .index(label: l[2]))
	lazy var emptyZone: RSS = Rope(with: .zone(under: c[0], .empty))
	lazy var indexInEmptyZone: RSS = Rope(with:
	    .zone(under: c[0], .index(label: l[0])))
	lazy var text: RSS = Rope(with: .text("a"))
	func testEmptyZone() {
		XCTAssert(emptyZone.startIndex < emptyZone.endIndex)
	}
	func testEmpty() {
		XCTAssert(empty.startIndex == empty.endIndex)
	}
	func testIndexInEmptyZone() {
		let rope = indexInEmptyZone
		XCTAssert(rope.startIndex < rope.endIndex)
		let index = RSS.Index.interior(of: rope, label: l[0])
		XCTAssert(index < rope.endIndex)
		XCTAssert(rope.startIndex < index)
	}
	func testAllIndices() {
		XCTAssert(allIndices.startIndex == allIndices.endIndex)
		let indices = l.map { label in
		    RSS.Index.interior(of: allIndices, label: label)
		}
		for index in indices {
			XCTAssert(index == allIndices.endIndex)
			XCTAssert(allIndices.startIndex == index)
		}
	}
	func testText() {
		XCTAssert(text.startIndex < text.endIndex)
	}
}

class HasSingleIndex : XCTestCase {
	let c = [RWZC(), RWZC(), RWZC()]
	let l = Array<Label>((0..<3).map { _ in Label() })
	lazy var empty: RSS = Rope(with: .empty)
	lazy var allIndices: RSS = Rope(with:
	    .index(label: l[0]), .index(label: l[1]), .index(label: l[2]))
	lazy var emptyZone: RSS = Rope(with: .zone(under: c[0], .empty))
	lazy var text: RSS = Rope(with: .text("a"))
	func testEmptyZone() {
		XCTAssert(!emptyZone.hasSingleIndex)
	}
	func testEmpty() {
		XCTAssert(empty.hasSingleIndex)
	}
	func testAllIndices() {
		XCTAssert(allIndices.hasSingleIndex)
	}
	func testText() {
		XCTAssert(!text.hasSingleIndex)
	}
}

class IndexedZoneBase : XCTestCase {
	let c = [RWZC(), RWZC(), RWZC()]
	static let indices: ClosedRange<Int> = 0...9
	let l = Array<Label>(indices.map { _ in Label() })
	// (abc(def(ghi)))
	// 000000000000000
	//     1111111111
	//         22222
	// *(*abc*(*def*(*ghi*)*)*)*
	lazy var rope: RSS = Rope(with:
	    .nodes(.index(label: l[0]),
		   .zone(under: c[0],
		       .index(label: l[1]),
		       .text("abc"),
		       .index(label: l[2]),
		       .zone(under: c[1],
		           .index(label: l[3]),
		           .text("def"),
		           .index(label: l[4]),
		           .zone(under: c[2],
		               .index(label: l[5]),
		               .text("ghi"),
		               .index(label: l[6])),
		           .index(label: l[7])),
		           .index(label: l[8])),
		   .index(label: l[9])))
	func testInsertingFirstIndex() {
		XCTAssert(rope.firstIndex(inZone: c[0]) ==
		          .interior(of: rope, label: l[1]))
		XCTAssert(rope.firstIndex(inZone: c[1]) ==
		          .interior(of: rope, label: l[3]))
		XCTAssert(rope.firstIndex(inZone: c[2]) ==
		          .interior(of: rope, label: l[5]))
	}
	func testInsertingLastIndex() {
		XCTAssert(rope.lastIndex(inZone: c[0]) ==
		          .interior(of: rope, label: l[8]))
		XCTAssert(rope.lastIndex(inZone: c[1]) ==
		          .interior(of: rope, label: l[7]))
		XCTAssert(rope.lastIndex(inZone: c[2]) ==
		          .interior(of: rope, label: l[6]))
	}
	func testInsertingIndexAfter() {
		XCTAssert(rope.index(afterZone: c[0]) ==
		          .interior(of: rope, label: l[9]))
		XCTAssert(rope.index(afterZone: c[1]) ==
		          .interior(of: rope, label: l[8]))
		XCTAssert(rope.index(afterZone: c[2]) ==
		          .interior(of: rope, label: l[7]))
	}
	func testInsertingIndexBefore() {
		XCTAssert(rope.index(beforeZone: c[0]) ==
		          .interior(of: rope, label: l[0]))
		XCTAssert(rope.index(beforeZone: c[1]) ==
		          .interior(of: rope, label: l[2]))
		XCTAssert(rope.index(beforeZone: c[2]) ==
		          .interior(of: rope, label: l[4]))
	}
}

class ConstructEmbeddedSelections : XCTestCase {
	let c = [RWZC(), RWZC(), RWZC(), RWZC()]
	let text: Substring = "pqrstu"
	func testInitialSelection() {
		var nonEmptyRanges: [(Int, Int)] = []
		let length = text.count

		nonEmptyRanges = (0..<length).flatMap { first in
			((first + 1)..<length).map { last in (first, last) }
		}
		for ((first, last), (innerFirst, innerLast)) in
		    nonEmptyRanges ⨯ nonEmptyRanges {
			let changes = ChangeList<RSS>()
			let pqrstu: RSS = Rope(with: .text(text))
			let outerParts = (head: text.prefix(first),
			             middle: text.prefix(last).dropFirst(first),
			             tail: text.dropFirst(last))
			let innerParts = (head: text.prefix(innerFirst),
			    middle:
			        text.prefix(innerLast).dropFirst(innerFirst),
			    tail: text.dropFirst(innerLast))

			let outer = text.unitRange(for: outerParts.middle)
			let inner = text.unitRange(for: innerParts.middle)

			let outerRange = Range(outer, within: pqrstu.units)
			let innerRange = Range(inner, within: pqrstu.units)
			let after: NSS =
			    .nodes(.text(outerParts.head),
				   .zone(c[0], .text(outerParts.middle)),
				   .text(outerParts.tail))
			// pqr*stu*
			guard let (range, narrow, wide) =
			    try? pqrstu.directedSelection(outerRange) else {
				XCTAssert(false, "\(outer) not found")
				return
			}
			XCTAssert(range == outerRange)
			XCTAssert(narrow == nil)
			XCTAssert(wide == nil)
			XCTAssertNoThrow(try pqrstu.setController(c[0],
			    on: range, undoList: changes),
			    "could not set controller")
			XCTAssert(pqrstu.node ~ after,
			    "expected \(pqrstu.node) ~ \(after)")

			if outer.contains(inner) {
				guard let (range2, narrow2, wide2) =
				    try? pqrstu.directedSelection(innerRange)
				    else {
					XCTAssert(false,
					    "\(inner) not found")
					return
				}
				XCTAssert(narrow2 == c[0])
				XCTAssert(wide2 == c[0])
				XCTAssertNoThrow(try pqrstu.setController(c[1],
				    on: range2, undoList: changes),
				    "could not set controller")
			} else if outer.intersects(inner) &&
			          !inner.contains(outer) {
				XCTAssertThrowsError(
				    try pqrstu.setController(c[1],
				        on: innerRange, undoList: changes),
				    "could not set controller, " +
				    "outer range \(outer), inner \(inner)")
			}
		}
	}
}

class DirectSelection : XCTestCase {
	let c = [RWZC(), RWZC(), RWZC(), RWZC()]
	// (a)b(c)
	lazy var abc: RSS = Rope(with:
	    .nodes(.zone(under: c[0], .text("a")), .text("b"),
	           .zone(under: c[1], .text("c"))))
	// w(x(y(z)))
	lazy var wxyz: RSS = Rope(with:
	    .nodes(.text("w"),
		   .zone(under: c[0],
		       .text("x"),
		       .zone(under: c[1],
		           .text("y"),
			   .zone(under: c[2], .text("z"))))))
	// (()(a)b(cd)ef)
	lazy var abcdef: RSS = Rope(with:
	    .zone(under: c[0],
	        .zone(under: c[1], .empty),
	        .zone(under: c[2], .text("a")),
	        .text("b"),
	        .zone(under: c[3], .text("cd")),
	        .text("ef")))
	// (p(q)r)(s)
	lazy var pqrs: RSS = Rope(with:
	    .nodes(.zone(under: c[0], .text("p"),
	           .zone(under: c[1], .text("q")),
		   .text("r")),
		   .zone(under: c[2], .text("s"))))
	func testDirectedWxyz0() {
		let start = wxyz.startIndex
		let end = wxyz.index(before: wxyz.endIndex)
		// *w(x(y(z))*)
		guard let (range, narrow, wide) =
		    try? wxyz.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// *w(x(y(z)))*
		let l = start
		let r = wxyz.endIndex
		XCTAssert(range == l..<r)
		XCTAssert(narrow == nil)
		XCTAssert(wide == nil)
	}
	func testDirectedWxyz1() {
		let start = wxyz.index(after: wxyz.startIndex)
		let end = wxyz.index(before: wxyz.endIndex)
		// w*(x(y(z))*)
		guard let (range, narrow, wide) =
		    try? wxyz.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// w(*x(y(z))*)
		let indices = wxyz.indices
		let l = indices.index(wxyz.startIndex, offsetBy: 2)
		let r = end
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedWxyz2() {
		let indices = wxyz.indices
		let start = indices.index(wxyz.startIndex, offsetBy: 2)
		let end = wxyz.index(before: wxyz.endIndex)
		// w(*x(y(z))*)
		guard let (range, narrow, wide) =
		    try? wxyz.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// w(*x(y(z))*)
		let l = start
		let r = end
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedWxyz3() {
		let indices = wxyz.indices
		let start = indices.index(wxyz.startIndex, offsetBy: 3)
		let end = wxyz.index(before: wxyz.endIndex)
		// w(x*(y(z))*)
		guard let (range, narrow, wide) =
		    try? wxyz.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// w(x(*y(z)*))
		let l = indices.index(wxyz.startIndex, offsetBy: 4)
		let r = indices.index(wxyz.endIndex, offsetBy: -2)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[1])
		XCTAssert(wide == c[0])
	}
	func testDirectedWxyz4() {
		let indices = wxyz.indices
		let start = indices.index(wxyz.startIndex, offsetBy: 4)
		let end = wxyz.index(before: wxyz.endIndex)
		// w(x(*y(z))*)
		guard let (range, narrow, wide) =
		    try? wxyz.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// w(x(*y(z)*))
		let l = start
		let r = indices.index(wxyz.endIndex, offsetBy: -2)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[1])
		XCTAssert(wide == c[0])
	}
	func testDirectedWxyz5() {
		let indices = wxyz.indices
		let start = indices.index(wxyz.startIndex, offsetBy: 5)
		let end = wxyz.index(before: wxyz.endIndex)
		// w(x(y*(z))*)
		guard let (range, narrow, wide) =
		    try? wxyz.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// w(x(y(*z*)))
		let l = indices.index(wxyz.startIndex, offsetBy: 6)
		let r = indices.index(wxyz.endIndex, offsetBy: -3)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[2])
		XCTAssert(wide == c[0])
	}
	func testDirectedWxyz6() {
		let indices = wxyz.indices
		let start = indices.index(wxyz.startIndex, offsetBy: 6)
		let end = indices.index(wxyz.endIndex, offsetBy: -4)
		// w(x(y(**z)))
		guard let (range, narrow, wide) =
		    try? wxyz.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// w(x(y(**z)))
		let l = start
		let r = end
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[2])
		XCTAssert(wide == c[0])
	}
	func testDirectedAbc1() {
		let start = abc.index(after: abc.startIndex)
		let end = abc.index(before: abc.endIndex)
		// (*a)b(c*)
		guard let (range, narrow, wide) =
		    try? abc.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// *(a)b(c)*
		XCTAssert(range == abc.startIndex..<abc.endIndex)
		XCTAssert(narrow == nil)
		XCTAssert(wide == nil)
	}
	func testTightenPqrs1() {
		let indices = pqrs.indices
		let start = indices.index(pqrs.startIndex, offsetBy: 2)
		let end = indices.index(pqrs.endIndex, offsetBy: -3)
		// (p*(q)r)*(s)
		guard let (range, lctlrs, rctlrs) =
		    try? pqrs.tightenedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (p*(q)r*)(s)
		let l = start
		let r = indices.index(pqrs.endIndex, offsetBy: -4)
		XCTAssert(range == l..<r)
		XCTAssert(lctlrs[...] == c[..<1])
		XCTAssert(rctlrs[...] == c[..<1])
	}
	func testDirectedPqrs1() {
		let indices = pqrs.indices
		let start = indices.index(pqrs.startIndex, offsetBy: 2)
		let end = indices.index(pqrs.endIndex, offsetBy: -3)
		// (p*(q)r)*(s)
		guard let (range, narrow, wide) =
		    try? pqrs.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (p*(q)r*)(s)
		let l = start
		let r = indices.index(pqrs.endIndex, offsetBy: -4)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedPqrs2() {
		let indices = pqrs.indices
		let start = indices.index(pqrs.startIndex, offsetBy: 2)
		let end = indices.index(pqrs.endIndex, offsetBy: -5)
		// (p*(q)*r)(s)
		guard let (range, narrow, wide) =
		    try? pqrs.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (p(*q*)r)(s)
		let l = indices.index(pqrs.startIndex, offsetBy: 3)
		let r = indices.index(pqrs.endIndex, offsetBy: -6)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[1])
		XCTAssert(wide == c[0])
	}
	func testDirectedAbcdef1() {
		let indices = abcdef.indices
		let start = indices.index(abcdef.startIndex, offsetBy: 3)
		let end = indices.index(abcdef.endIndex, offsetBy: -5)
		// (()*(a)b(c*d)ef)
		guard let (range, narrow, wide) =
		    try? abcdef.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (()*(a)b(c*d)ef)
		XCTAssert(range == start..<end)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedAbcdef2() {
		let indices = abcdef.indices
		let start = indices.index(abcdef.startIndex, offsetBy: 4)
		let end = indices.index(abcdef.endIndex, offsetBy: -5)
		// (()(*a)b(c*d)ef)
		guard let (range, narrow, wide) =
		    try? abcdef.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (()*(a)b(c*d)ef)
		let l = indices.index(abcdef.startIndex, offsetBy: 3)
		let r = end
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedAbcdef3() {
		let indices = abcdef.indices
		let start = indices.index(abcdef.startIndex, offsetBy: 5)
		let end = indices.index(abcdef.endIndex, offsetBy: -5)
		// (()(a*)b(c*d)ef)
		guard let (range, narrow, wide) =
		    try? abcdef.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (()(a*)b(c*d)ef)
		let l = start
		let r = end
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedAbcdef4() {
		let indices = abcdef.indices
		let start = indices.index(abcdef.startIndex, offsetBy: 2)
		let end = indices.index(abcdef.endIndex, offsetBy: -5)
		// ((*)(a)b(c*d)ef)
		guard let (range, narrow, wide) =
		    try? abcdef.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (*()(a)b(c*d)ef)
		let l = indices.index(abcdef.startIndex, offsetBy: 1)
		let r = end
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedAbcdef5() {
		let indices = abcdef.indices
		let start = indices.index(abcdef.startIndex, offsetBy: 2)
		let end = indices.index(abcdef.endIndex, offsetBy: -4)
		// ((*)(a)b(cd*)ef)
		guard let (range, narrow, wide) =
		    try? abcdef.directedSelection(start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (*()(a)b(cd)*ef)
		let l = indices.index(abcdef.startIndex, offsetBy: 1)
		let r = indices.index(abcdef.endIndex, offsetBy: -3)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
}

class IndexOffsetBy : XCTestCase {
	let c = [RWZC(), RWZC(), RWZC(), RWZC()]
	// (()(a)b(cd)ef)
	lazy var abcdef: RSS = Rope(with:
		    .zone(under: c[0],
		        .zone(under: c[1], .empty),
		        .zone(under: c[2], .text("a")),
			.text("b"),
		        .zone(under: c[3], .text("cd")),
			.text("ef")))
	func testStart() {
		let indices = abcdef.indices
		XCTAssert(indices.index(abcdef.startIndex, offsetBy: 0) ==
		          abcdef.startIndex)
	}
	func testEnd() {
		let indices = abcdef.indices
		XCTAssert(indices.index(abcdef.endIndex, offsetBy: 0) ==
		          abcdef.endIndex)
	}
	func testStartToEnd() {
		let indices = abcdef.indices
		XCTAssert(indices.index(abcdef.startIndex, offsetBy: 14) ==
		          abcdef.endIndex)
	}
	func testEndToStart() {
		let indices = abcdef.indices
		XCTAssert(indices.index(abcdef.endIndex, offsetBy: -14) ==
		          abcdef.startIndex)
	}
	func testAll() {
		let indices = abcdef.indices
		for i in 0...14 {
			let l = indices.index(abcdef.startIndex, offsetBy: i)
			let r = indices.index(abcdef.endIndex, offsetBy: i - 14)
			XCTAssert(l == r)
		}
	}
}

class SegmentingAtZone : XCTestCase {
	let c: [RWZC] = [RWZC(), RWZC(), RWZC(), RWZC()]
	var _zoneInZone: NSS? = nil
	var _zoneInCenter: NSS? = nil
	var _multipleZonesInCenter: NSS? = nil
	var _zoneOnLeft: NSS? = nil
	var _zoneOnRight: NSS? = nil
	var _innermostZone: NSS? = nil
	var _innerZone: NSS? = nil
	var _otherZone: NSS? = nil
	lazy var innermostZone: NSS = .zone(under: c[2], .text("ghi"))
	lazy var innerZone: NSS =
	    .zone(under: c[1], .text("def"), innermostZone)
	lazy var otherZone: NSS = .zone(under: c[3], .text("012"))
	lazy var zoneInZone: NSS =
	    .zone(under: c[0], .text("abc"), innerZone)
	lazy var zoneOnRight: NSS = .nodes(.text("abc"), innerZone)
	lazy var zoneOnLeft: NSS = .nodes(innerZone, .text("jkl"))
	lazy var multipleZonesInCenter: NSS =
	    .nodes(.text("abc"), innerZone, otherZone, .text("jkl"))
	lazy var zoneInCenter: NSS =
	    .nodes(.text("abc"), innerZone, .text("jkl"))
	func testSegmentingEmbeddedZones() {
		XCTAssertThrowsError(try zoneInZone.segmenting(atZone: c[1]))
		XCTAssertThrowsError(try zoneInZone.segmenting(atZone: c[2]))
	}
	func testSegmentingZone() {
		guard let (l, (ctlr, inner), r) =
		    try? zoneInZone.segmenting(atZone: c[0]) else {
			XCTFail("no such zone controller")
			return
		}
		XCTAssert(l == .empty)
		XCTAssert(.zone(ctlr, inner) == zoneInZone)
		XCTAssert(r == .empty)
	}
	func testSegmentingZoneOnLeft() {
		guard let (l, (ctlr, inner), r) =
		    try? zoneOnLeft.segmenting(atZone: c[1]) else {
			XCTFail("no such zone controller")
			return
		}
		XCTAssert(l == .empty)
		XCTAssert(.zone(ctlr, inner) == innerZone)
		XCTAssert(r == .text("jkl"))
	}
	func testSegmentingZoneOnRight() {
		guard let (l, (ctlr, inner), r) =
		    try? zoneOnRight.segmenting(atZone: c[1])
		else {
			XCTFail("no such zone controller")
			return
		}
		XCTAssert(l == .text("abc"))
		XCTAssert(.zone(ctlr, inner) == innerZone)
		XCTAssert(r == .empty)
	}
	func testSegmentingZoneInCenter() {
		guard let (l, (ctlr, inner), r) =
		    try? zoneInCenter.segmenting(atZone: c[1])
		else {
			XCTFail("no such zone controller")
			return
		}
		XCTAssert(l == .text("abc"))
		XCTAssert(.zone(ctlr, inner) == innerZone)
		XCTAssert(r == .text("jkl"))
	}
	func testSegmentingMultipleZonesInCenter1() {
		guard let (l, (ctlr, inner), r) =
		    try? multipleZonesInCenter.segmenting(atZone: c[1])
		else {
			XCTFail("no such zone controller")
			return
		}
		XCTAssert(l == .text("abc"))
		XCTAssert(.zone(ctlr, inner) == innerZone)
		XCTAssert(r == .nodes(otherZone, .text("jkl")))
	}
	func testSegmentingMultipleZonesInCenter2() {
		guard let (l, (ctlr, inner), r) =
		    try? multipleZonesInCenter.segmenting(atZone: c[3])
		else {
			XCTFail("no such zone controller")
			return
		}
		XCTAssert(l == .nodes(.text("abc"), innerZone))
		XCTAssert(.zone(ctlr, inner) == otherZone)
		XCTAssert(r == .text("jkl"))
	}
}

class RopeIndexedControllerPaths: NestedZoneBase {
	var _expectations: [[Label]]? = nil
	var expectations: [[Label]] {
		if let olde = _expectations {
			return olde
		}
		// (abc(def(ghi)))
		// 000000000000000
		//     1111111111
		//         22222
		let newe: [[Label]] = [
		    [],			// *(abc(def(ghi)))
		    [c[0]],		// (*abc(def(ghi)))
		    [c[0]],		// (a*bc(def(ghi)))
		    [c[0]],		// (ab*c(def(ghi)))
		    [c[0]],		// (abc*(def(ghi)))
		    [c[0], c[1]],	// (abc(*def(ghi)))
		    [c[0], c[1]],	// (abc(d*ef(ghi)))
		    [c[0], c[1]],	// (abc(de*f(ghi)))
		    [c[0], c[1]],	// (abc(def*(ghi)))
		    [c[0], c[1], c[2]],	// (abc(def(*ghi)))
		    [c[0], c[1], c[2]],	// (abc(def(g*hi)))
		    [c[0], c[1], c[2]],	// (abc(def(gh*i)))
		    [c[0], c[1], c[2]],	// (abc(def(ghi*)))
		    [c[0], c[1]],	// (abc(def(ghi)*))
		    [c[0]],		// (abc(def(ghi))*)
		    []]			// (abc(def(ghi)))*
		_expectations = newe
		return newe
	}
	func testControllerPaths() {
		for (i, expected) in zip(rope.indices, expectations) {
			XCTAssert(try rope.zonesEnclosing(i) == expected)
		}
	}
	func testForwardClimbIn() {
		let down: [Bool] = [
		    true,	// *(abc(def(ghi)))
		    false,	// (*abc(def(ghi)))
		    false,	// (a*bc(def(ghi)))
		    false,	// (ab*c(def(ghi)))
		    true,	// (abc*(def(ghi)))
		    false,	// (abc(*def(ghi)))
		    false,	// (abc(d*ef(ghi)))
		    false,	// (abc(de*f(ghi)))
		    true,	// (abc(def*(ghi)))
		    false,	// (abc(def(*ghi)))
		    false,	// (abc(def(g*hi)))
		    false,	// (abc(def(gh*i)))
		    false,	// (abc(def(ghi*)))
		    false,	// (abc(def(ghi)*))
		    false,	// (abc(def(ghi))*)
		    false]	// (abc(def(ghi)))*
		for (i, expected) in zip(rope.indices, down) {
			let j = rope.index(after: i, climbing: .in)
			XCTAssert((j != nil) == expected)
		}
	}
	func testBackwardClimbIn() {
		let down: [Bool] = [
		    false,	// *(abc(def(ghi)))
		    false,	// (*abc(def(ghi)))
		    false,	// (a*bc(def(ghi)))
		    false,	// (ab*c(def(ghi)))
		    false,	// (abc*(def(ghi)))
		    false,	// (abc(*def(ghi)))
		    false,	// (abc(d*ef(ghi)))
		    false,	// (abc(de*f(ghi)))
		    false,	// (abc(def*(ghi)))
		    false,	// (abc(def(*ghi)))
		    false,	// (abc(def(g*hi)))
		    false,	// (abc(def(gh*i)))
		    false,	// (abc(def(ghi*)))
		    true,	// (abc(def(ghi)*))
		    true,	// (abc(def(ghi))*)
		    true]	// (abc(def(ghi)))*
		for (i, expected) in zip(rope.indices, down) {
			let j = rope.index(before: i, climbing: .in)
			XCTAssert((j != nil) == expected)
		}
	}
	func testForwardClimbOut() {
		let down: [Bool] = [
		    false,	// *(abc(def(ghi)))
		    false,	// (*abc(def(ghi)))
		    false,	// (a*bc(def(ghi)))
		    false,	// (ab*c(def(ghi)))
		    false,	// (abc*(def(ghi)))
		    false,	// (abc(*def(ghi)))
		    false,	// (abc(d*ef(ghi)))
		    false,	// (abc(de*f(ghi)))
		    false,	// (abc(def*(ghi)))
		    false,	// (abc(def(*ghi)))
		    false,	// (abc(def(g*hi)))
		    false,	// (abc(def(gh*i)))
		    true,	// (abc(def(ghi*)))
		    true,	// (abc(def(ghi)*))
		    true,	// (abc(def(ghi))*)
		    false]	// (abc(def(ghi)))*
		for (i, expected) in zip(rope.indices, down) {
			let j = rope.index(after: i, climbing: .out)
			XCTAssert((j != nil) == expected)
		}
	}
	func testBackwardClimbOut() {
		let down: [Bool] = [
		    false,	// *(abc(def(ghi)))
		    true,	// (*abc(def(ghi)))
		    false,	// (a*bc(def(ghi)))
		    false,	// (ab*c(def(ghi)))
		    false,	// (abc*(def(ghi)))
		    true,	// (abc(*def(ghi)))
		    false,	// (abc(d*ef(ghi)))
		    false,	// (abc(de*f(ghi)))
		    false,	// (abc(def*(ghi)))
		    true,	// (abc(def(*ghi)))
		    false,	// (abc(def(g*hi)))
		    false,	// (abc(def(gh*i)))
		    false,	// (abc(def(ghi*)))
		    false,	// (abc(def(ghi)*))
		    false,	// (abc(def(ghi))*)
		    false]	// (abc(def(ghi)))*
		for (i, expected) in zip(rope.indices, down) {
			let j = rope.index(before: i, climbing: .out)
			XCTAssert((j != nil) == expected)
		}
	}
}

class UTF16IndexedControllerPaths: XCTestCase {
	let c = [RWZC(), RWZC(), RWZC()]
	lazy var tree: NSS = .zone(under: c[0],
		                     .nodes(.text("abc"),
				     .zone(under: c[1],
				         .text("def"),
					 .zone(under: c[2], .text("ghi")))))
	lazy var expectations: [[Label]] =
	    [[c[0]],
	     [c[0]],
	     [c[0]],
	     [c[0], c[1]],
	     [c[0], c[1]],
	     [c[0], c[1]],
	     [c[0], c[1], c[2]],
	     [c[0], c[1], c[2]],
	     [c[0], c[1], c[2]],
	     [c[0], c[1], c[2]]]
	let indices: [Offset] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	func testControllerPaths() {
		for (i, expected) in zip(indices, expectations) {
			XCTAssert(tree.zonesEnclosing(i) == expected)
		}
	}
}

class WholeRangeUsingRopeIndices: XCTestCase {
	let ctlr = RWZC()
	lazy var r: RSS = Rope(with: 
	    .nodes(.zone(under: ctlr, .text("abc")),
		   .text("def")))
	func testLookupByRange() {
		XCTAssert(r.nests[r.startIndex..<r.endIndex] ~ r.node)
	}
}

class ZonesOpeningClosing : XCTestCase {
	let simpleCtlr = RWZC()
	let empty: RSS = Rope()
	lazy var simple: RSS = Rope(with: .zone(under: simpleCtlr, .empty))
	let c = [RWZC(),	// 0
	         RWZC(),	// 1
	         RWZC(),	// 2
	         RWZC(),	// 3
	         RWZC(),	// 4
	         RWZC(),	// 5
	         RWZC(),	// 6
	         RWZC(),	// 7
	         RWZC(),	// 8
	         RWZC()]	// 9
	// ()(a(b)c())(((def)))(((g)h)i)
	// 00111111111444444444777777777
	//     222 33  5555555  888888
	//              66666    999
	lazy var cplx: RSS = Rope(with:
	    .nodes(.zone(under: c[0], .empty),
		   .zone(under: c[1],
		       .text("a"),
		       .zone(under: c[2], .text("b")),
		       .text("c"),
		       .zone(under: c[3], .empty)),
		   .zone(under: c[4],
		       .zone(under: c[5],
			   .zone(under: c[6],
			       .text("def")))),
		   .zone(under: c[7],
		       .zone(under: c[8],
			   .zone(under: c[9], .text("g")),
			   .text("h")),
		       .text("i"))))
	func testClosingEmpty() {
		XCTAssert(try empty.zonesClosing(at: empty.startIndex) == [])
		XCTAssert(try empty.zonesClosing(at: empty.endIndex) == [])
	}
	func testOpeningEmpty() {
		XCTAssert(try empty.zonesOpening(at: empty.startIndex) == [])
		XCTAssert(try empty.zonesOpening(at: empty.endIndex) == [])
	}
	func testClosingSimple() {
		let middle = simple.index(after: simple.startIndex)
		XCTAssert(try simple.zonesClosing(at: simple.startIndex) ==
		    [])
		XCTAssert(try simple.zonesClosing(at: middle) == [simpleCtlr])
		XCTAssert(try simple.zonesClosing(at: simple.endIndex) == [])
	}
	func testOpeningSimple() {
		let middle = simple.index(after: simple.startIndex)
		XCTAssert(try simple.zonesOpening(at: simple.startIndex) ==
		    [])
		XCTAssert(try simple.zonesOpening(at: middle) == [simpleCtlr])
		XCTAssert(try simple.zonesClosing(at: simple.endIndex) == [])
	}
	func testClosingComplex() {
		let expectations = [
		    [],		// *()(a(b)c())(((def)))(((g)h)i)
		    [0],	// (*)(a(b)c())(((def)))(((g)h)i)
		    [],		// ()*(a(b)c())(((def)))(((g)h)i)
		    [],		// ()(*a(b)c())(((def)))(((g)h)i)
		    [],		// ()(a*(b)c())(((def)))(((g)h)i)
		    [],		// ()(a(*b)c())(((def)))(((g)h)i)
		    [2],	// ()(a(b*)c())(((def)))(((g)h)i)
		    [],		// ()(a(b)*c())(((def)))(((g)h)i)
		    [],		// ()(a(b)c*())(((def)))(((g)h)i)
		    [1, 3],	// ()(a(b)c(*))(((def)))(((g)h)i)
		    [1],	// ()(a(b)c()*)(((def)))(((g)h)i)
		    [],		// ()(a(b)c())*(((def)))(((g)h)i)
		    [],		// ()(a(b)c())(*((def)))(((g)h)i)
		    [],		// ()(a(b)c())((*(def)))(((g)h)i)
		    [],		// ()(a(b)c())(((*def)))(((g)h)i)
		    [],		// ()(a(b)c())(((d*ef)))(((g)h)i)
		    [],		// ()(a(b)c())(((de*f)))(((g)h)i)
		    [4, 5, 6],	// ()(a(b)c())(((def*)))(((g)h)i)
		    [4, 5],	// ()(a(b)c())(((def)*))(((g)h)i)
		    [4],	// ()(a(b)c())(((def))*)(((g)h)i)
		    [],		// ()(a(b)c())(((def)))*(((g)h)i)
		    [],		// ()(a(b)c())(((def)))(*((g)h)i)
		    [],		// ()(a(b)c())(((def)))((*(g)h)i)
		    [],		// ()(a(b)c())(((def)))(((*g)h)i)
		    [9],	// ()(a(b)c())(((def)))(((g*)h)i)
		    [],		// ()(a(b)c())(((def)))(((g)*h)i)
		    [8],	// ()(a(b)c())(((def)))(((g)h*)i)
		    [],		// ()(a(b)c())(((def)))(((g)h)*i)
		    [7],	// ()(a(b)c())(((def)))(((g)h)i*)
		    []]		// ()(a(b)c())(((def)))(((g)h)i)*

		for (idx, expected) in zip(cplx.indices, expectations) {
			guard let found = try? cplx.zonesClosing(at: idx)
			    else {
				XCTFail("no such index")
				continue
			}
			XCTAssert(found == expected.map { i in c[i] },
			          "found \(found) expected \(expected)")
		}
	}
	func testOpeningComplex() {
		let expectations = [
				// ()(a(b)c())(((def)))(((g)h)i)
				// 00111111111444444444777777777
				//     222 33  5555555  888888
				//              66666    999
		    [],		// *()(a(b)c())(((def)))(((g)h)i)
		    [0],	// (*)(a(b)c())(((def)))(((g)h)i)
		    [],		// ()*(a(b)c())(((def)))(((g)h)i)
		    [1],	// ()(*a(b)c())(((def)))(((g)h)i)
		    [],		// ()(a*(b)c())(((def)))(((g)h)i)
		    [2],	// ()(a(*b)c())(((def)))(((g)h)i)
		    [],		// ()(a(b*)c())(((def)))(((g)h)i)
		    [],		// ()(a(b)*c())(((def)))(((g)h)i)
		    [],		// ()(a(b)c*())(((def)))(((g)h)i)
		    [3],	// ()(a(b)c(*))(((def)))(((g)h)i)
		    [],		// ()(a(b)c()*)(((def)))(((g)h)i)
		    [],		// ()(a(b)c())*(((def)))(((g)h)i)
		    [4],	// ()(a(b)c())(*((def)))(((g)h)i)
		    [4, 5],	// ()(a(b)c())((*(def)))(((g)h)i)
		    [4, 5, 6],	// ()(a(b)c())(((*def)))(((g)h)i)
		    [],		// ()(a(b)c())(((d*ef)))(((g)h)i)
		    [],		// ()(a(b)c())(((de*f)))(((g)h)i)
		    [],		// ()(a(b)c())(((def*)))(((g)h)i)
		    [],		// ()(a(b)c())(((def)*))(((g)h)i)
		    [],		// ()(a(b)c())(((def))*)(((g)h)i)
		    [],		// ()(a(b)c())(((def)))*(((g)h)i)
		    [7],	// ()(a(b)c())(((def)))(*((g)h)i)
		    [7, 8],	// ()(a(b)c())(((def)))((*(g)h)i)
		    [7, 8, 9],	// ()(a(b)c())(((def)))(((*g)h)i)
		    [],		// ()(a(b)c())(((def)))(((g*)h)i)
		    [],		// ()(a(b)c())(((def)))(((g)*h)i)
		    [],		// ()(a(b)c())(((def)))(((g)h*)i)
		    [],		// ()(a(b)c())(((def)))(((g)h)*i)
		    [],		// ()(a(b)c())(((def)))(((g)h)i*)
		    []]		// ()(a(b)c())(((def)))(((g)h)i)*

		for (idx, expected) in zip(cplx.indices, expectations) {
			guard let found = try? cplx.zonesOpening(at: idx)
			    else {
				XCTFail("no such index")
				continue
			}
			XCTAssert(found == expected.map { i in c[i] },
			          "found \(found) expected \(expected)")
		}
	}
}

class EmptyishRopeIndices : XCTestCase {
	let one: RSS = Rope(with: .zone(under: RWZC(), .empty))
	let two: RSS = Rope(with:
	    .nodes(.zone(under: RWZC(), .empty),
		   .zone(under: RWZC(), .empty)))
	let empty: RSS = Rope()
	func testStartIndexEqualsEndIndex() {
		XCTAssert(empty.startIndex == empty.endIndex)
	}
	func testStartIndexOneEmptyZone() {
		let indices = one.indices
		XCTAssert(one.startIndex != one.endIndex)
		XCTAssert(one.index(after: one.startIndex) != one.endIndex)
		XCTAssert(indices.index(one.startIndex, offsetBy: 2) ==
		    one.endIndex)
	}
	func testEndIndexOneEmptyZone() {
		let indices = one.indices
		XCTAssert(one.index(before: one.endIndex) != one.startIndex)
		XCTAssert(indices.index(one.endIndex, offsetBy: -2) ==
		    one.startIndex)
	}
	func testStartIndexTwoEmptyZones() {
		XCTAssert(two.startIndex != two.endIndex)
		let indices = two.indices
		XCTAssert(two.index(after: two.startIndex) != two.endIndex)
		XCTAssert(indices.index(two.startIndex, offsetBy: 2) !=
		    two.endIndex)
		XCTAssert(indices.index(two.startIndex, offsetBy: 3) !=
		    two.endIndex)
		XCTAssert(indices.index(two.startIndex, offsetBy: 4) ==
		    two.endIndex)
	}
	func testEndIndexTwoEmptyZones() {
		let indices = two.indices
		XCTAssert(two.index(before: two.endIndex) != two.startIndex)
		XCTAssert(indices.index(two.endIndex, offsetBy: -2) !=
		    two.startIndex)
		XCTAssert(indices.index(two.endIndex, offsetBy: -3) !=
		    two.startIndex)
		XCTAssert(indices.index(two.endIndex, offsetBy: -4) ==
		    two.startIndex)
	}
}

class RangesThreeRopeIndicesWide: XCTestCase {
	let ctlr = RWZC()
	lazy var expectations: [NSS] = [
	    .zone(under: ctlr, .text("ab")),
	    .zone(under: ctlr, .text("abc")),
	    .zone(under: ctlr, .text("bc")),
	    .nodes(.zone(under: ctlr, .text("c")),
		 .text("d")),
	    .nodes(.zone(under: ctlr, .empty), .text("de")),
	    .text("def")]
	lazy var r: RSS = Rope(with:
	    .nodes(.zone(under: ctlr, .text("abc")),
		   .text("def")))
	func testLookupByRangesForward() {
		var prev = r.startIndex
		for (idx, expected) in zip(r.indices.dropFirst(3),
			                   expectations) {
			let found = r.nests[prev..<idx]
			prev = r.index(after: prev)
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
	func testLookupByRangesBackward() {
		var prev = r.endIndex
		for (idx, expected) in zip(r.indices.reversed().dropFirst(2),
			                   expectations.reversed()) {
			let found = r.nests[idx..<prev]
			prev = r.index(before: prev)
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
}

class RangesTwoRopeIndicesWide: XCTestCase {
	let ctlr = RWZC()
	lazy var expectations: [NSS] = [
	    .zone(under: ctlr, .text("a")),
	    .zone(under: ctlr, .text("ab")),
	    .zone(under: ctlr, .text("bc")),
	    .zone(under: ctlr, .text("c")),
	    .nodes(.zone(under: ctlr, .empty),
		   .text("d")),
	    .text("de"),
	    .text("ef")]
	lazy var r: RSS = Rope(with:
	    .nodes(.zone(under: ctlr, .text("abc")), .text("def")))
	func testLookupByRangesForward() {
		var prev = r.startIndex
		for (idx, expected) in zip(r.indices.dropFirst(2),
			                   expectations) {
			let found = r.nests[prev..<idx]
			prev = r.index(after: prev)
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
	func testLookupByRangesBackward() {
		var prev = r.endIndex
		for (idx, expected) in zip(r.indices.reversed().dropFirst(1),
			                   expectations.reversed()) {
			let found = r.nests[idx..<prev]
			prev = r.index(before: prev)
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
}

class RangesOneRopeIndexWide: XCTestCase {
	let ctlr = RWZC()
	lazy var expectations: [NSS] = [
	    .empty,
	    .zone(under: ctlr, .empty),
	    .zone(under: ctlr, .text("a")),
	    .zone(under: ctlr, .text("b")),
	    .zone(under: ctlr, .text("c")),
	    .zone(under: ctlr, .empty),
	    .text("d"),
	    .text("e"),
	    .text("f")]
	lazy var r: RSS = Rope(with:
	    .nodes(.zone(under: ctlr, .text("abc")), .text("def")))
	func testLookupByRangesForward() {
		var prev = r.startIndex
		for (idx, expected) in zip(r.indices, expectations) {
			let found = r.nests[prev..<idx]
			prev = idx
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
	func testLookupByRangesBackward() {
		var prev = r.endIndex
		for (idx, expected) in zip(r.indices.reversed(),
                                           expectations.reversed()) {
			let found = r.nests[idx..<prev]
			prev = idx
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
}

class ConvertRopeIndicesToUnitOffsets : XCTestCase {
	let ctlr = [RWZC(), RWZC()]
	lazy var r: RSS  = Rope(with:
	    .zone(under: ctlr[0], .text("abc")),
	    .text("def"),
	    .zone(under: ctlr[1], .text("ghi")))
	let expectations: [Int] = [
		0, // .(abc)def(ghi)
		0, // (.abc)def(ghi)
		1, // (a.bc)def(ghi)
		2, // (ab.c)def(ghi)
		3, // (abc.)def(ghi)
		3, // (abc).def(ghi)
		4, // (abc)d.ef(ghi)
		5, // (abc)de.f(ghi)
		6, // (abc)def.(ghi)
		6, // (abc)def(.ghi)
		7, // (abc)def(g.hi)
		8, // (abc)def(gh.i)
		9, // (abc)def(ghi.)
		9  // (abc)def(ghi).
		]
	func testEveryIndex() {
		XCTAssert(expectations.count == r.indices.count + 1,
		    "expectations.count \(expectations.count) != r.indices.count \(r.indices.count + 1)")
		for (index, expectation) in zip(r.indices + [r.endIndex],
			                        expectations) {
			XCTAssert(try expectation == r.offset(of: index,
			                                      on: \.units),
			    "\(expectation) != " +
			    "r.offset(of: \(index), on: \\.units)")
		}
	}
}

class LookupUsingRopeIndicesDerivedFromUTF16Offsets: XCTestCase {
	let ctlr = RWZC()
	lazy var expectations: [NSS] = [
	    .zone(under: ctlr, .text("a")),
	    .zone(under: ctlr, .text("b")),
	    .zone(under: ctlr, .text("c")),
	    .text("d"),
	    .text("e"),
	    .text("f")]
	lazy var r: RSS  = Rope(with:
	    .zone(under: ctlr, .text("abc")),
		   .text("def"))
	lazy var nests = r.nests
	func testIterateElements() {
		let nests = r.nests
		for (i, expected) in expectations.enumerated() {
			let ofs = i
			let idx = RSS.Index(abutting: ofs, on: .right,
			    within: r.units)
			let found = nests[idx]
			XCTAssert(found == expected,
			    "found \(found) expected \(expected)")
		}
	}
	func testEndIndex() {
		let ofs = expectations.count
		let idx = RSS.Index(abutting: ofs, on: .right, within: r.units)
		XCTAssertThrowsError(try nests.element(at: idx))
	}
}

class ZoneElementLookupUsingRopeIndices: XCTestCase {
	let ctlr = RWZC()
	lazy var expectations: [NSS] = [
	    .empty,
	    .zone(under: ctlr, .text("a")),
	    .zone(under: ctlr, .text("b")),
	    .zone(under: ctlr, .text("c")),
	    .zone(under: ctlr, .empty),
	    .text("d"),
	    .text("e"),
	    .text("f")]
	lazy var r: RSS = Rope(with:
	    .nodes(.zone(under: ctlr, .text("abc")),
		   .text("def")))
	lazy var nests = r.nests
	func testElementsCount() {
		XCTAssert(nests.count == expectations.count)
	}
	func testIterateElements() {
		for (found, expected) in zip(nests, expectations) {
			XCTAssert(found == expected)
		}
	}
	func testIndicesCount() {
		XCTAssert(r.indices.count == expectations.count)
	}
	func testLookupByIndices() {
		for (idx, expected) in zip(r.indices, expectations) {
			let found = nests[idx]
			XCTAssert(found == expected)
		}
	}
	func testStepIndicesForward() {
		var idx = r.startIndex
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		idx = r.index(after: idx)
		XCTAssert(idx == r.endIndex)
	}
	func testStepIndicesBackward() {
		var idx = r.endIndex
		idx = r.index(before: idx)
		idx = r.index(before: idx)
		idx = r.index(before: idx)
		idx = r.index(before: idx)
		idx = r.index(before: idx)
		idx = r.index(before: idx)
		idx = r.index(before: idx)
		idx = r.index(before: idx)
		XCTAssert(idx == r.startIndex)
	}
	func testEndIndices() {
		let idx = r.endIndex
		XCTAssertThrowsError(try nests.element(at: idx))
	}
}

class BasicElementLookupUsingRopeIndex: XCTestCase {
	let rope1: RSS = Rope(content: "abc")
	let rope2: RSS = Rope(content: "def")
	lazy var nests1 = rope1.nests
	lazy var nests2 = rope2.nests
	func testStartIndex() {
		let idx1 = rope1.startIndex
		let idx2 = rope2.startIndex
		XCTAssert(nests1[idx1].content == "a")
		XCTAssert(nests2[idx2].content == "d")
	}
	func testSecondIndex() {
		let idx1 = rope1.index(after: rope1.startIndex)
		let idx2 = rope2.index(after: rope2.startIndex)
		XCTAssert(nests1[idx1].content == "b")
		XCTAssert(nests2[idx2].content == "e")
	}
	func testThirdIndex() {
		let idx1 = rope1.index(after:
		           rope1.index(after: rope1.startIndex))
		let idx2 = rope2.index(after:
		           rope2.index(after: rope2.startIndex))
		XCTAssert(nests1[idx1].content == "c")
		XCTAssert(nests2[idx2].content == "f")
		XCTAssert(rope1.index(after: idx1) == rope1.endIndex)
		XCTAssert(rope2.index(after: idx2) == rope2.endIndex)
	}
	func testThreeLeftOfEndIndex() {
		let idx1 = rope1.index(before:
		           rope1.index(before:
			   rope1.index(before: rope1.endIndex)))
		let idx2 = rope2.index(before:
		           rope2.index(before:
			   rope2.index(before: rope2.endIndex)))
		XCTAssert(nests1[idx1].content == "a")
		XCTAssert(nests2[idx2].content == "d")
		XCTAssert(idx1 == rope1.startIndex)
		XCTAssert(idx2 == rope2.startIndex)
	}
	func testTwoLeftOfEndIndex() {
		let idx1 = rope1.index(before:
		           rope1.index(before: rope1.endIndex))
		let idx2 = rope2.index(before:
		           rope2.index(before: rope2.endIndex))
		XCTAssert(nests1[idx1].content == "b")
		XCTAssert(nests2[idx2].content == "e")
	}
	func testOneLeftOfEndIndex() {
		let idx1 = rope1.index(before: rope1.endIndex)
		let idx2 = rope2.index(before: rope2.endIndex)
		XCTAssert(nests1[idx1].content == "c")
		XCTAssert(nests2[idx2].content == "f")
		XCTAssert(rope1.index(after: idx1) == rope1.endIndex)
		XCTAssert(rope2.index(after: idx2) == rope2.endIndex)
	}
	func testEndIndex() {
		let idx1 = rope1.endIndex
		let idx2 = rope2.endIndex
		XCTAssertThrowsError(try nests1.element(at: idx1))
		XCTAssertThrowsError(try nests2.element(at: idx2))
	}
}

class CompareDisparateRopeIndices: XCTestCase {
	let rope1: RSS = Rope(content: "abc")
	let rope2: RSS = Rope(content: "def")
	func testStartIndices() {
		let idx1 = rope1.startIndex
		let idx2 = rope2.startIndex
		XCTAssertThrowsError(try idx1.isLessThan(idx2))
		XCTAssertThrowsError(try idx1.aliases(idx2))
	}
	func testSecondIndices() {
		let idx1 = rope1.index(after: rope1.startIndex)
		let idx2 = rope2.index(after: rope2.startIndex)
		XCTAssertThrowsError(try idx1.isLessThan(idx2))
		XCTAssertThrowsError(try idx1.aliases(idx2))
	}
	func testEndIndices() {
		let idx1 = rope1.endIndex
		let idx2 = rope2.endIndex
		XCTAssertThrowsError(try idx1.isLessThan(idx2))
		XCTAssertThrowsError(try idx1.aliases(idx2))
	}
}

class FibonacciTests : XCTestCase {
	func testFibonacciByIndex() {
		XCTAssert([0, 1, 2, 3, 4, 5].map {
			i in fibonacci(index: i)
		} == [0, 1, 1, 2, 3, 5])
	}

	func testFibonacciGenerator() {
		// Produce the Fibonacci sequence, 0th through 5th element.
		let arr = Fibonacci(through: 5).reduce([]) {
			(arr: [UInt], elt: UInt) -> [UInt] in
				arr + [elt]
		}
		XCTAssert(arr == [0, 1, 1, 2, 3, 5])
	}
}

class LabelHolding : XCTestCase {

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
	func testReleaseLabel() {
		var h = Label()
		let w = Weak(h)
		h = Label()
		XCTAssert(w.get() == nil)
	}

	/* Test the expectation that if the only remaining references to an
	 * object are the references held by Weak structs,
	 * w and x, then trying to retrieve the object with w.get() and
	 * x.get() yields nil.
	 */
	func testReleaseTwoLabels() {
		var h = Label()
		let w = Weak(h)
		let x = Weak(h)
		h = Label()
		XCTAssert(w.get() == nil)
		XCTAssert(x.get() == nil)
	}

	/* Test the expectation that if there is a second reference to an
	 * object, o, that is also held by a Weak struct, w, then the
	 * object retrieved by w.get() is o.
	 */
	func testHoldLabel() {
		let h = Label()
		let w = Weak(h)
		XCTAssert(w.get() == h)
	}

	func testStepAndHoldIndex() {
		let first: NSS = .text("abc")
		let label = Label()
		guard case .step(let second) =
		    first.inserting(label, after: .rightStep) else {
			XCTFail("inserting(_,after:) failed")
			return
		}
		XCTAssert(second.leaves.map { (x: RSS.Node) -> Bool in
			if case .index(let w) = x {
				return w.get() == label
			} else {
				return false
			}
		}[1])
	}

	func testStepAndReleaseIndex() {
		let first: NSS = .text("abc")
		var label = Label()
		guard case .step(let second) =
		    first.inserting(label, after: .rightStep) else {
			XCTFail("inserting(_,after:) failed")
			return
		}
		label = Label()
		XCTAssert(!second.leaves.map { (x: RSS.Node) -> Bool in
			if case .index(let w) = x {
				return w.get() == label
			} else {
				return false
			}
		}[1])
	}

	static func isIndex(_ n: RSS.Node) -> Bool {
		if case .index(_) = n {
			return true
		}
		return false
	}

	static func isNilIndex(_ n: RSS.Node) -> Bool {
		if case .index(let w) = n {
			return w.get() == nil
		}
		return false
	}

	func testCleanedHoldingIndices() {
		let emptyRope: RSS = Rope(content: "abcdefghijkl")
		var indices: [RSS.Index]? = []

		for i in emptyRope.indices {
			indices?.append(i)
		}
		print(emptyRope.node)
		XCTAssert(emptyRope.node.cleaned()?.leaves.filter(
		    LabelHolding.isIndex).count == 13)
	}

	func testCleanedReleasingIndices() {
		let emptyRope: RSS = Rope(content: "abcdefghijkl")
		var indices: [RSS.Index]? = []

		for i in emptyRope.indices {
			indices?.append(i)
		}
		print(emptyRope.node)
		indices = nil
		XCTAssert(emptyRope.node.cleaned()?.leaves.filter(
		    LabelHolding.isIndex).count == 2)
	}

	func testReleasingIndices() {
		let rope: RSS = Rope(content: "abcdefghijkl")
		var indices: [RSS.Index]? = []

		for i in rope.indices {
			indices?.append(i)
		}

		indices = nil
		print(rope.node.leaves)
		print(rope.node.leaves.filter(LabelHolding.isNilIndex))

		XCTAssert(rope.node.leaves.filter(
		    LabelHolding.isNilIndex).count == 12)
	}

	func testPerformanceExample() {
		// This is an example of a performance test case.
		self.measure {
			// Put the code you want to measure the time of here.
		}
	}
}

class NodeSubropes : XCTestCase {
	let rope: RSS =
	    Rope(with: .nodes(.text("abc"), .text("defgh"), .text("ijkl")))

	func testFullContent() {
		XCTAssert(rope.node.content == "abcdefghijkl")
	}

	func testLeadingSubnode() {
		let section = rope.units[0..<3]
		XCTAssert(section == "abc")
	}

	func testCrossingFirstTwoSubnodes() {
		let section = rope.units[0..<5]
		XCTAssert(section == "abcde")
	}

	func testSecondSubnode() {
		let section = rope.units[3..<8]
		XCTAssert(section == "defgh")
	}

	func testTrailingTwoSubnodes() {
		let section = rope.units[3..<12]
		XCTAssert(section == "defghijkl")
	}

	func testCrossingLastTwoSubnodes() {
		let section = rope.units[4..<9]
		XCTAssert(section == "efghi")
	}
}

extension NSAttributedString.Key {
	public static let odor: Self = Self(rawValue: "odor")
	public static let taste: Self = Self(rawValue: "taste")
	public static let personality: Self = Self(rawValue: "personality")
}

class NodeAttributes : XCTestCase {
	typealias Key = NSAttributedString.Key
	static let frontAttrs: Attributes = [Key.odor : true]
	static let middleAttrs: Attributes = [Key.taste : true]
	static let backAttrs: Attributes = [Key.personality : true]
	static let newAttrs: Attributes =
	    [Key.odor : false, Key.personality : false]
	static let abc: NSS = .text("abc", attributes: frontAttrs)
	static let defgh: NSS = .text("defgh", attributes: middleAttrs)
	static let ijkl: NSS = .text("ijkl", attributes: backAttrs)
	// (abc(def(ghi)))
	// 000000000000000
	//     1111111111
	//         22222
	let n: NSS = .nodes(abc, defgh, ijkl)
	lazy var rope: RSS = Rope(with: n)
	let ctlr = RWZC()
	lazy var contained: RSS = Rope(with: .zone(under: ctlr, n))
	func testFrontAttributes() {
		let (attrs, range) = n.attributes(at: 0, on: \.units)
		XCTAssert(Self.frontAttrs ~ attrs)
		XCTAssert(range == 0..<3)
	}
	func testMiddleAttributes() {
		let (attrs, range) = n.attributes(at: 3, on: \.units)
		XCTAssert(Self.middleAttrs ~ attrs)
		XCTAssert(range == 3..<8)
	}
	func testBackAttributes() {
		let (attrs, range) = n.attributes(at: 8, on: \.units)
		XCTAssert(Self.backAttrs ~ attrs)
		XCTAssert(range == 8..<12)
	}
	func testLastAttributes() {
		let (attrs, range) = n.attributes(at: 11, on: \.units)
		XCTAssert(Self.backAttrs ~ attrs)
		XCTAssert(range == 8..<12)
	}
	func testSettingFrontAndMiddleAttributes() {
		let ir = Range(0..<8, within: rope.units)
		XCTAssertNoThrow { [rope] in
			let newn = try rope.node.settingAttributes(
			    NodeAttributes.newAttrs, range: ir)
			let (attrs, frontRange) =
			    newn.attributes(at: 0, on: \.units)
			XCTAssert(Self.newAttrs ~ attrs)
			XCTAssert(frontRange == 0..<3)
			let (_, middleRange) =
			    newn.attributes(at: 3, on: \.units)
			XCTAssert(middleRange == 3..<8)
		}
	}
	static func helpTestSettingCentralAttributes(_ oldr: RSS) {
		let ir = Range(2..<9, within: oldr.units)
		XCTAssertNoThrow { [oldr] in
			let newn = try oldr.node.settingAttributes(
			    NodeAttributes.newAttrs, range: ir)

			let (frontAttrs, frontRange) =
			    newn.attributes(at: 0, on: \.units)
			XCTAssert(frontRange == 0..<2)
			XCTAssert(Self.frontAttrs ~ frontAttrs)

			let (midAttrs1, midRange1) =
			    newn.attributes(at: 2, on: \.units)
			XCTAssert(midRange1 == 2..<3)
			XCTAssert(Self.newAttrs ~ midAttrs1)

			let (midAttrs2, midRange2) =
			    newn.attributes(at: 3, on: \.units)
			XCTAssert(midRange2 == 3..<8)
			XCTAssert(Self.newAttrs ~ midAttrs2)

			let (midAttrs3, midRange3) =
			    newn.attributes(at: 8, on: \.units)
			XCTAssert(midRange3 == 8..<9)
			XCTAssert(Self.newAttrs ~ midAttrs3)

			let (backAttrs, backRange) =
			    newn.attributes(at: 9, on: \.units)
			XCTAssert(backRange == 9..<12)
			XCTAssert(Self.backAttrs ~ backAttrs)
		}
	}
	func testSettingCentralAttributes() {
		NodeAttributes.helpTestSettingCentralAttributes(rope)
	}
	func testSettingCentralAttributesWithZone() {
		NodeAttributes.helpTestSettingCentralAttributes(contained)
	}
	func testSettingBackAttributes() {
		let ir = Range(8..<12, within: rope.units)
		XCTAssertNoThrow { [self] in
			let newn = try rope.node.settingAttributes(
			    NodeAttributes.newAttrs, range: ir)
			let (attrs, range) = newn.attributes(at: 8, on: \.units)
			XCTAssert(Self.newAttrs ~ attrs)
			XCTAssert(range == 8..<12,
			    "actual range \(range) " +
			    "newn \(newn.debugDescription) " +
			    "n \(n.debugDescription)")
		}
	}
	func testSettingLastAttributes() {
		let ir = Range(11..<12, within: rope.units)
		XCTAssertNoThrow { [rope] in
			let newn = try rope.node.settingAttributes(
			    NodeAttributes.newAttrs, range: ir)
			let (attrs, range) =
			    newn.attributes(at: 11, on: \.units)
			XCTAssert(Self.newAttrs ~ attrs)
			XCTAssert(range == 11..<12)
			let (_, abuttingRange) =
			    newn.attributes(at: 8, on: \.units)
			XCTAssert(abuttingRange == 8..<11)
		}
	}
}

class AppendInsertRemoveReplace : XCTestCase {
	func testReplace() {
		
		// let rope: RSS = Rope(content:
		// "This is the original content.")
		let str: String = "This is the original content."
	
		print(str.firstIndex(of: " is") ?? -1)
	}
}

class CommonPrefix : XCTestCase {
	let l: [Int] = [1, 2, 3, 4, 5]
	let r: [Int] = [1, 2, 3, 4, 5]
	let p: [Int] = [3, 4, 5]
	let q: [Int] = [1, 2, 3, 4, 5]
	let a: [Int] = [1, 2, 3]
	let b: [Int] = [1, 2, 3, 4, 5]
	let empty: [Int] = []
	func testWholeMatch() {
		XCTAssert(commonPrefix(l, r) == l)
	}
	func testNoMatch() {
		XCTAssert(commonPrefix(p, q) == empty)
	}
	func testPartialMatch() {
		XCTAssert(commonPrefix(a, b) == [1, 2, 3])
	}
	func testEmpty() {
		XCTAssert(commonPrefix(empty, empty) == empty)
	}
	func testEmpty2() {
		XCTAssert(commonPrefix(empty, r) == empty)
	}
	func testEmpty3() {
		XCTAssert(commonPrefix(l, empty) == empty)
	}
}

class LabelSets : XCTestCase {
	func testInit() {
		let set = LabelSet()
		XCTAssert(set.zoneCount == 0)
		XCTAssert(set.indexCount == 0)
	}
	func testSeqInit() {
		let ids: [Label.Id] = [.zone(1), .index(2)]
		let set = LabelSet(ids)
		XCTAssert(set.zoneCount == 1)
		XCTAssert(set.indexCount == 1)
	}
	func testLiteralInit() {
		let set: [LabelSet] = [[.zone(0), .index(0)],
		                       [.zone(0), .index(0), .index(1)],
		                       [.zone(0), .index(0)],
		                       [.zone(0), .zone(1), .index(0)]]
		XCTAssert(set[0].zoneCount == 1)
		XCTAssert(set[0].indexCount == 1)
		XCTAssert(set[1].zoneCount == 1)
		XCTAssert(set[1].indexCount == 2)
		XCTAssert(set[2].zoneCount == 1)
		XCTAssert(set[2].indexCount == 1)
		XCTAssert(set[3].zoneCount == 2)
		XCTAssert(set[3].indexCount == 1)
	}
	func testUnion() {
		let set: [LabelSet] = [[.zone(0), .index(0)],
		                       [.zone(0), .index(0), .index(1)],
		                       [.zone(0), .index(0)],
		                       [.zone(0), .zone(1), .index(0)]]
		XCTAssert(set[0].union(set[1]).zoneCount == 1)
		XCTAssert(set[0].union(set[1]).indexCount == 2)
		XCTAssert(set[1].union(set[2]).zoneCount == 1)
		XCTAssert(set[1].union(set[2]).indexCount == 2)
		XCTAssert(set[2].union(set[3]).zoneCount == 2)
		XCTAssert(set[2].union(set[3]).indexCount == 1)
		var p = set[0], q = set[1]
		p.formUnion(q)
		XCTAssert(p.zoneCount == 1)
		XCTAssert(p.indexCount == 2)
	}
	func testIntersection() {
		let overlap: [LabelSet] = [[.zone(0), .index(0)],
		                           [.zone(0), .index(0), .index(1)],
		                           [.zone(0), .index(0)],
		                           [.zone(0), .zone(1), .index(0)]]

		XCTAssert(overlap[0].intersection(overlap[1]).zoneCount == 1)
		XCTAssert(overlap[0].intersection(overlap[1]).indexCount == 1)
		XCTAssert(overlap[1].intersection(overlap[2]).zoneCount == 1)
		XCTAssert(overlap[1].intersection(overlap[2]).indexCount == 1)
		XCTAssert(overlap[2].intersection(overlap[3]).zoneCount == 1)
		XCTAssert(overlap[2].intersection(overlap[3]).indexCount == 1)

		let disj: [LabelSet] = [[.zone(0), .index(0)],
		                        [.zone(4), .index(4), .index(8)],
		                        [.zone(8), .index(12)],
		                        [.zone(12), .zone(16), .index(16)]]

		XCTAssert(disj[0].intersection(disj[1]).zoneCount == 0)
		XCTAssert(disj[0].intersection(disj[1]).indexCount == 0)
		XCTAssert(disj[1].intersection(disj[2]).zoneCount == 0)
		XCTAssert(disj[1].intersection(disj[2]).indexCount == 0)
		XCTAssert(disj[2].intersection(disj[3]).zoneCount == 0)
		XCTAssert(disj[2].intersection(disj[3]).indexCount == 0)
	}
	func testSymmetricDifference() {
		let overlap: [LabelSet] = [
		    [.zone(0), .index(0)],		// 0
		    [.zone(0), .index(0), .index(1)],	// 1
		    [.zone(0), .index(0)],		// 2
		    [.zone(0), .zone(1), .index(0)]]	// 3

		let diff0 = overlap[0].symmetricDifference(overlap[1])
		XCTAssert(diff0.zoneCount == 0)
		XCTAssert(diff0.indexCount == 1)

		let diff1 = overlap[1].symmetricDifference(overlap[2])
		XCTAssert(diff1.zoneCount == 0)
		XCTAssert(diff1.indexCount == 1)

		let diff2 = overlap[2].symmetricDifference(overlap[3])
		XCTAssert(diff2.zoneCount == 1)
		XCTAssert(diff2.indexCount == 0)

		let disj: [LabelSet] = [
		    [.zone(0), .index(0)],		// 0
		    [.zone(4), .index(4), .index(8)],	// 1
		    [.zone(8), .index(12)],		// 2
		    [.zone(12), .zone(16), .index(16)]]	// 3

		let diff3 = disj[0].symmetricDifference(disj[1])

		XCTAssert(diff3.zoneCount == 2)
		XCTAssert(diff3.indexCount == 3)

		let diff4 = disj[1].symmetricDifference(disj[2])

		XCTAssert(diff4.zoneCount == 2)
		XCTAssert(diff4.indexCount == 3)

		let diff5 = disj[2].symmetricDifference(disj[3])

		XCTAssert(diff5.zoneCount == 3)
		XCTAssert(diff5.indexCount == 2)
	}
	func testInsert() {
		var set: LabelSet = [.index(0)]

		XCTAssert(set.indexCount == 1)
		XCTAssert(set.insert(.index(0)) == (false, .index(0)))
		XCTAssert(set.indexCount == 1)
		XCTAssert(set.insert(.index(1)) == (true, .index(1)))
		XCTAssert(set.indexCount == 2)
		XCTAssert(set.zoneCount == 0)
	}
	func testRemove() {
		var set: LabelSet = [.index(0)]

		XCTAssert(set.indexCount == 1)
		XCTAssert(set.remove(.index(1)) == nil)
		XCTAssert(set.indexCount == 1)
		XCTAssert(set.remove(.index(0)) == .index(0))
		XCTAssert(set.indexCount == 0)
		XCTAssert(set.zoneCount == 0)
		XCTAssert(set.indexCount == 0)
	}
	func testUpdate() {
		var set: LabelSet = [.index(0)]

		XCTAssert(set.indexCount == 1)
		XCTAssert(set.update(with: .index(0)) == .index(0))
		XCTAssert(set.indexCount == 1)
		XCTAssert(set.update(with: .index(1)) == nil)
		XCTAssert(set.indexCount == 2)
		XCTAssert(set.zoneCount == 0)
	}
}

class TightenSelection: NestedZoneBase {
	public func testBothTighten1() {
		// *(abc(def(ghi)))*
		let start = rope.startIndex
		let end = rope.endIndex
		let outer = start..<end
		let result = try? rope.tightenedSelection(outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightenedSelection()")
			return
		}
		let l = rope.index(after: rope.startIndex)
		let r = rope.index(before: rope.endIndex)
		// (*abc(def(ghi))*)
		XCTAssert(tightened == l..<r)
		XCTAssert(lctlrs[...] == c[..<1])
		XCTAssert(rctlrs[...] == c[..<1])
	}
	public func testBothTighten2() {
		// (abc*(def(ghi))*)
		let indices = rope.indices
		let start = indices.index(rope.startIndex, offsetBy: 4)
		let end = indices.index(rope.endIndex, offsetBy: -1)
		let outer = start..<end
		let result = try? rope.tightenedSelection(outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightenedSelection()")
			return
		}
		let l = indices.index(rope.startIndex, offsetBy: 5)
		let r = indices.index(rope.endIndex, offsetBy: -2)
		// (abc(*def(ghi)*))
		XCTAssert(tightened == l..<r)
		XCTAssert(lctlrs[...] == c[..<2])
		XCTAssert(rctlrs[...] == c[..<2])
	}
	public func testBothTighten3() {
		let indices = rope.indices
		// (abc(def*(ghi)*))
		let start = indices.index(rope.startIndex, offsetBy: 8)
		let end = indices.index(rope.endIndex, offsetBy: -2)
		let outer = start..<end
		let result = try? rope.tightenedSelection(outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightenedSelection()")
			return
		}
		let l = indices.index(rope.startIndex, offsetBy: 9)
		let r = indices.index(rope.endIndex, offsetBy: -3)
		// (abc(def(*ghi*)))
		XCTAssert(tightened == l..<r)
		XCTAssert(lctlrs == c)
		XCTAssert(rctlrs == c)
	}
	/* All index positions:
	 *   *(abc(def(ghi)))
	 *   (*abc(def(ghi)))
	 *   (a*bc(def(ghi)))
	 *   (ab*c(def(ghi)))
	 *   (abc*(def(ghi)))
	 *   (abc(*def(ghi)))
	 *   (abc(d*ef(ghi)))
	 *   (abc(de*f(ghi)))
	 *   (abc(def*(ghi)))
	 *   (abc(def(*ghi)))
	 *   (abc(def(g*hi)))
	 *   (abc(def(gh*i)))
	 *   (abc(def(ghi*)))
	 *   (abc(def(ghi)*))
	 *   (abc(def(ghi))*)
	 *   (abc(def(ghi)))*
	 */
	public func testLeftTighten1() {
		let indices = rope.indices
		// *(abc(def(ghi))*)
		let start = rope.startIndex
		let end = indices.index(rope.endIndex, offsetBy: -1)
		let outer = start..<end
		let result = try? rope.tightenedSelection(outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightenedSelection()")
			return
		}
		let l = indices.index(rope.startIndex, offsetBy: 1)
		let r = indices.index(rope.endIndex, offsetBy: -1)
		// (*abc(def(ghi))*)
		XCTAssert(tightened == l..<r)
		XCTAssert(lctlrs[...] == c[..<1])
		XCTAssert(rctlrs[...] == c[..<1])
	}
	public func testRightTighten1() {
		let indices = rope.indices
		// (*abc(def(ghi)))*
		let start = indices.index(rope.startIndex, offsetBy: 1)
		let end = rope.endIndex
		let outer = start..<end
		let result = try? rope.tightenedSelection(outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightenedSelection()")
			return
		}
		let l = start
		let r = indices.index(rope.endIndex, offsetBy: -1)
		// (*abc(def(ghi))*)
		XCTAssert(tightened == l..<r)
		XCTAssert(lctlrs[...] == c[..<1])
		XCTAssert(rctlrs[...] == c[..<1])
	}
	public func testUntightenable1() {
		let indices = rope.indices
		// (*abc(def(ghi))*)
		let start = indices.index(rope.startIndex, offsetBy: 1)
		let end = indices.index(rope.endIndex, offsetBy: -1)
		let outer = start..<end
		let result = try? rope.tightenedSelection(outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightenedSelection()")
			return
		}
		// (*abc(def(ghi))*)
		XCTAssert(tightened == outer)
		XCTAssert(lctlrs[...] == c[..<1])
		XCTAssert(rctlrs[...] == c[..<1])
	}
}

class CompareIndicesAndEndComplicatedRopes: NestedZoneBase {
	func testEquals() {
		let indices = rope.indices
		XCTAssert(
		    rope.index(after: rope.index(before: rope.endIndex)) ==
		    rope.endIndex)
		XCTAssert(
		    indices.index(indices.index(rope.endIndex, offsetBy: -2),
		               offsetBy: 2) == rope.endIndex)
	}
	/* Delete right of index(pqrs.endIndex, offsetBy: n < 0): an interior
	 * index is no longer interior if the content to its right was
	 * deleted.
	 */
	func testEdits() {
		let pqrs = RSS(content: "pqrs")
		let indices = pqrs.indices
		let idx = indices.index(pqrs.endIndex, offsetBy: -2)
		let undoList = ChangeList<RSS>()
		try! pqrs.replace(Range(2..<4, within: pqrs.units),
		    with: Substring(""), undoList: undoList)
		XCTAssert(idx == pqrs.endIndex)
	}
}

class CompareIndicesAndStartComplicatedRopes: NestedZoneBase {
	func testEquals() {
		let ixs = rope.indices
		XCTAssert(
		    ixs.index(before: ixs.index(after: ixs.startIndex)) ==
		    ixs.startIndex)
		XCTAssert(
		    ixs.index(ixs.index(ixs.startIndex, offsetBy: 2),
		              offsetBy: -2) == ixs.startIndex)
	}
	/* Delete left of index(pqrs.startIndex, offsetBy: n > 0): an interior
	 * index is no longer interior if the content to its left was
	 * deleted.
	 */
	func testEdits() {
		let pqrs = RSS(content: "pqrs")
		let indices = pqrs.indices
		let idx = indices.index(pqrs.startIndex, offsetBy: 2)
		let undoList = ChangeList<RSS>()
		try! pqrs.replace(Range(0..<2, within: pqrs.units),
		    with: Substring(""), undoList: undoList)
		XCTAssert(idx == pqrs.startIndex)
	}
}

class CompareDisparateIndicesComplicatedRopes: NestedZoneBase {
	/* All index positions:
	 *   *(abc(def(ghi)))
	 *   (*abc(def(ghi)))
	 *   (a*bc(def(ghi)))
	 *   (ab*c(def(ghi)))
	 *   (abc*(def(ghi)))
	 *   (abc(*def(ghi)))
	 *   (abc(d*ef(ghi)))
	 *   (abc(de*f(ghi)))
	 *   (abc(def*(ghi)))
	 *   (abc(def(*ghi)))
	 *   (abc(def(g*hi)))
	 *   (abc(def(gh*i)))
	 *   (abc(def(ghi*)))
	 *   (abc(def(ghi)*))
	 *   (abc(def(ghi))*)
	 *   (abc(def(ghi)))*
	 */
	func testEquals() {
		let ixs = rope.indices
		XCTAssert(ixs.index(before: ixs.endIndex) ==
		    ixs.index(ixs.endIndex, offsetBy: -1))
		XCTAssert(
		    ixs.index(before: ixs.index(before: ixs.endIndex)) ==
		    ixs.index(ixs.endIndex, offsetBy: -2))
		XCTAssert(ixs.index(before: ixs.endIndex) ==
		    ixs.index(ixs.startIndex, offsetBy: 14))
		XCTAssert(ixs.index(after: ixs.startIndex) ==
		    ixs.index(ixs.endIndex, offsetBy: -14))
		let n = ixs.count + 1
		/* Test 3 times.  Each test leaves stale indices behind
		 * that should confound defective index comparison.
		 */
		for _ in 0..<3 {
			for step in 0..<n {
				let i1 = ixs.index(ixs.startIndex,
				                   offsetBy: step)
				let i2 = ixs.index(ixs.endIndex,
				    offsetBy: step - (n - 1))
				XCTAssert(i1 == i2)
			}
		}
	}
	func testLessThanGreaterThan() {
		/* Test 3 times.  Each test leaves stale indices behind
		 * that should confound defective index comparison.
		 */
		for _ in 0..<3 {
			let ixs = rope.indices
			let numberedIndices =
			    (ixs.indices + [rope.endIndex]).enumerated()
			for ((n1, idx1), (n2, idx2)) in
			    numberedIndices ⨯ numberedIndices {
				XCTAssert((n1 == n2) && (idx1 == idx2) ||
					  (n1 < n2) && (idx1 < idx2) ||
					  (n1 > n2) && (idx1 > idx2))
			}
		}
	}
}

class UndoRedo : XCTestCase {
	func testMultipleEditsAndCompleteUndo() throws {
		var contentHistory: [NSS.Content] = []
		let sentence = "All work and no play makes Jack a dull boy."
		let nl = "\n"
		let node: NSS = .text(sentence[...])
		let rope: RSS = Rope(with: node)
		var changesHistory: [ChangeList<RSS>] = []

		contentHistory.append(rope.node.content)
		var changes = ChangeList<RSS>()
		try rope.replace(rope.endIndex..<rope.endIndex, with: nl[...],
		    undoList: changes)
		changesHistory.append(changes)

		let line = sentence + nl

		contentHistory.append(rope.node.content)
		changes = ChangeList<RSS>()
		try rope.replace(rope.endIndex..<rope.endIndex, with: line[...],
		    undoList: changes)
		changesHistory.append(changes)

		contentHistory.append(rope.node.content)
		changes = ChangeList<RSS>()
		try rope.replace(rope.endIndex..<rope.endIndex, with: nl[...],
		    undoList: changes)
		changesHistory.append(changes)

		let paragraph = line + line + nl

		for _ in 0..<20 {
			contentHistory.append(rope.node.content)
			changes = ChangeList<RSS>()
			try rope.replace(rope.endIndex..<rope.endIndex,
			    with: paragraph[...], undoList: changes)
			changesHistory.append(changes)
		}

		let paragraphLength = (line + line + nl).units.count
		let offset = 4 * paragraphLength

		// nibble away at the 5th paragraph unit by unit starting
		// at the front
		for _ in 0..<paragraphLength {
			let ir = Range(offset..<offset+1, within: rope.units)

			contentHistory.append(rope.node.content)
			changes = ChangeList<RSS>()
			try rope.replace(ir, with: "", undoList: changes)
			changesHistory.append(changes)
		}

		while case (let content?, let changes?) =
		    (contentHistory.popLast(), changesHistory.popLast()) {
			let (_, _) = try changes.play(withTarget: rope)
			XCTAssert(content == rope.node.content,
			    "expected \(content) found \(rope.node.content)")
		}
		XCTAssert(contentHistory.isEmpty == changesHistory.isEmpty)
	}
}

class ZoneReplacementsBase : XCTestCase {
	typealias BeforeAfterCombo = (before: NSS, range: Range<Int>,
			    replacement: NSS.Content, expected: NSS,
			    changeInLength: Int)
	typealias CustomAssert = (_ expression: @autoclosure () throws -> NSS,
		                  _ message: @autoclosure () -> String) -> ()

	let rwc = [RWZC(), RWZC(), RWZC()]
	let roc = [ROZC(), ROZC(), ROZC()]
	// read-only inner zone, abc(defgh)ijk
	lazy var innerRO: NSS = .nodes(
	    .text("abc"),
	    .zone(under: roc[1], .text("defgh")),
	    .text("ijk"))
	// read-write inner zone, abc(defgh)ijk
	lazy var innerRW: NSS = .nodes(
	    .text("abc"),
	    .zone(under: rwc[1], .text("defgh")),
	    .text("ijk"))
	// read-only outer zones, (abc)defgh(ijk) 
	lazy var outerRO: NSS = .nodes(
	    .zone(under: roc[0], .text("abc")),
	    .text("defgh"),
	    .zone(under: roc[2], .text("ijk")))
	// read-write outer zones, (abc)defgh(ijk) 
	lazy var outerRW: NSS = .nodes(
	    .zone(under: rwc[0], .text("abc")),
	    .text("defgh"),
	    .zone(under: rwc[2], .text("ijk")))
	lazy var beforeAfterCombos: [BeforeAfterCombo] = [
		/* replace 0..<5 on abc(defgh)ijk with 01234, result
		 * 01234(fgh)ijk
		 */
		(innerRW, 0..<5, "01234", .nodes(
			    .text("01234"),
			    .zone(under: rwc[1], .text("fgh")),
			    .text("ijk")), 0),
		/* replace 0..<5 on (abc)defgh(ijk) with 01234, result
		 * (01234)fgh(ijk)
		 */
		(outerRW, 0..<5, "01234", .nodes(
			    .zone(under: rwc[0], .text("01234")),
			    .text("fgh"),
			    .zone(under: rwc[2], .text("ijk"))), 0),
		/* replace 2..<5 on abc(defgh)ijk with 01234, result
		 * ab01234(fgh)ijk
		 */
		(innerRW, 2..<5, "01234", .nodes(
			    .text("ab01234"),
			    .zone(under: rwc[1], .text("fgh")),
			    .text("ijk")), 2),
		/* replace 2..<5 on (abc)defgh(ijk) with 01234, result
		 * (ab01234)fgh(ijk)
		 */
		(outerRW, 2..<5, "01234", .nodes(
			    .zone(under: rwc[0], .text("ab01234")),
			    .text("fgh"),
			    .zone(under: rwc[2], .text("ijk"))), 2),
		/* replace 3..<8 on abc(defgh)ijk with 01234, result
		 * abc(01234)ijk
		 */
		(innerRW, 3..<8, "01234", .nodes(.text("abc"),
		    .zone(under: rwc[1], .text("01234")),
		    .text("ijk")), 0),
		/* replace 3..<8 on (abc)defgh(ijk) with 01234, result
		 * (abc)01234(ijk)
		 */
		(outerRW, 3..<8, "01234", .nodes(.zone(under: rwc[0], .text("abc")),
		    .text("01234"),
		    .zone(under: rwc[2], .text("ijk"))), 0),
		/* replace 7..<8 on abc(defgh)ijk with 01234, result
		 * abc(defg01234)ijk
		 */
		(innerRW, 7..<8, "01234", .nodes(.text("abc"),
		    .zone(under: rwc[1], .text("defg01234")),
		    .text("ijk")), 4),
		/* replace 7..<8 on (abc)defgh(ijk) with 01234, result
		 * (abc)defg01234(ijk)
		 */
		(outerRW, 7..<8, "01234", .nodes(.zone(under: rwc[0], .text("abc")),
		    .text("defg01234"),
		    .zone(under: rwc[2], .text("ijk"))), 4),
		/* replace 8..<9 on abc(defgh)ijk with 01234, result
		 * abc(defgh)01234jk
		 */
		(innerRW, 8..<9, "01234", .nodes(.text("abc"),
		    .zone(under: rwc[1], .text("defgh")),
		    .text("01234jk")), 4),
		/* replace 8..<9 on (abc)defgh(ijk) with 01234, result
		 * (abc)defgh(01234jk)
		 */
		(outerRW, 8..<9, "01234",
		    .nodes(.zone(under: rwc[0], .text("abc")),
		    .text("defgh"),
		    .zone(under: rwc[2], .text("01234jk"))), 4),
		/* replace 5..<9 on abc(defgh)ijk with 01234, result
		 * abc(de01234)jk
		 */
		(innerRW, 5..<9, "01234", .nodes(.text("abc"),
		    .zone(under: rwc[1], .text("de01234")),
		    .text("jk")), 1),
		/* replace 5..<9 on (abc)defgh(ijk) with 01234, result
		 * (abc)de01234(jk)
		 */
		(outerRW, 5..<9, "01234",
		    .nodes(.zone(under: rwc[0], .text("abc")),
		    .text("de01234"),
		    .zone(under: rwc[2], .text("jk"))), 1),
		/* replace 8..<11 on abc(defgh)ijk with 01234, result
		 * abc(defgh)01234
		 */
		(innerRW, 8..<11, "01234", .nodes(.text("abc"),
		    .zone(under: rwc[1], .text("defgh")),
		    .text("01234")), 2),
		/* replace 8..<11 on (abc)defgh(ijk) with 01234, result
		 * (abc)defgh(01234)
		 */
		(outerRW, 8..<11, "01234",
		    .nodes(.zone(under: rwc[0], .text("abc")),
		    .text("defgh"),
		    .zone(under: rwc[2], .text("01234"))), 2),
		/* replace 10..<11 on abc(defgh)ijk with 01234, result
		 * abc(defgh)ij01234
		 */
		(innerRW, 10..<11, "01234", .nodes(.text("abc"),
		    .zone(under: rwc[1], .text("defgh")),
		    .text("ij01234")), 4),
		/* replace 10..<11 on (abc)defgh(ijk) with 01234, result
		 * (abc)defgh(ij01234)
		 */
		(outerRW, 10..<11, "01234",
		    .nodes(.zone(under: rwc[0], .text("abc")),
		    .text("defgh"),
		    .zone(under: rwc[2], .text("ij01234"))), 4)
		]
	static func functionAssertingThrows(iff assert: Bool,
	    file: StaticString = #filePath, line: UInt = #line)
	    -> CustomAssert {
		return assert
		    ? { (f, message) in
			    try! XCTAssertThrowsError(f(),
				     "\(message()) should have thrown",
				     file: file, line: line)
		      } as CustomAssert
		    : { (f, message) in
			    try! XCTAssertNoThrow(f(),
				     "\(message()) should not have thrown",
				     file: file, line: line)
		      } as CustomAssert
	}
	func testReplaceInnerThrows() {
		let combinations = ([true, false] ⨯ (1...5)).flatMap {
		    [self] (ro, width) in
			(0...(innerRO.dimensions.units - width)).map { start in
			    (ro, width, start)
			}
		}
		for (ro, width, start) in combinations {
			let tree = ro ? innerRO : innerRW
			let rope: RSS = Rope(with: tree)
			let range = start..<(start + width)
			let ir = Range(range, within: rope.units)
			let overlaps = range.overlaps(3...7)
			let fails = ro && overlaps
			let assert = Self.functionAssertingThrows(iff: fails)
			let changes = ChangeList<NSS>()
			assert(try rope.node.replacing(
			    after: ir.lowerBound.label,
			    upTo: ir.upperBound.label, with: .text("x"),
			    undoList: changes),
			    "replacing \(width) at \(range)")
			assert(try rope.node.replacing(
			        after: ir.lowerBound.label,
			        upTo: ir.upperBound.label,
			        with: .text(("x" * width)[...]),
				undoList: changes),
			    "replacing \(width) at \(range)")
		}
	}
	func testReplaceOuterThrows() {
		let combinations = ([true, false] ⨯ (1...5)).flatMap {
		    [self] (ro, width) in
			(0...(outerRO.dimensions.units - width)).map { start in
			    (ro, width, start)
			}
		}
		for (ro, width, start) in combinations {
			let tree = ro ? outerRO : outerRW
			let rope: RSS = Rope(with: tree)
			let range = start..<(start + width)
			let ir = Range(range, within: rope.units)
			let overlaps = range.overlaps(0...2) ||
			               range.overlaps(8...10)
			let fails = ro && overlaps
			let assert = Self.functionAssertingThrows(iff: fails)
			let changes = ChangeList<NSS>()
			assert(try rope.node.replacing(
			                        after: ir.lowerBound.label,
						upTo: ir.upperBound.label,
						with: .text("x"),
						undoList: changes),
			    "replacing \(width) at \(range)")
			assert(try rope.node.replacing(
			        after: ir.lowerBound.label,
			        upTo: ir.upperBound.label,
			        with: .text(("x" * width)[...]),
				undoList: changes),
			    "replacing \(width) at \(range)")
		}
	}
	func testReplacementResults() {
		let changes = ChangeList<NSS>()

		for (before, range, replacement, expected, _) in
		    beforeAfterCombos {
			let rope: RSS = Rope(with: before)
			let ir = Range(range, within: rope.units)
			guard let after = try? rope.node.replacing(
			    after: ir.lowerBound.label,
			    upTo: ir.upperBound.label,
			    with: .text(replacement),
			    undoList: changes) else {
				XCTFail("labeling for replacement failed")
				return
			}
			guard let (undone, _) =
			    try? changes.play(withTarget: after) else {
				XCTFail("playing replacement failed")
				return
			}
			XCTAssert(expected ~ after, "\(expected) !~ \(after)")
			XCTAssert(undone ~ rope.node,
			    "\(undone) !~ \(rope.node)")
		}
	}
	func testReplacementChangeIndications() {
		for (before, range, replacement, _, changeInLength) in
		    beforeAfterCombos {
			let rope: RSS = Rope(with: before)
			var change: (range: Range<Int>,
			             changeInLength: Int) =
			    (range: 0..<0, changeInLength: Int.min)
			rope.axisDelegates[\.units] =
			    AnyRopeDelegate<RSS.Content>(
			        didChange: {
				    (range, delta) in
					change.range = range
					change.changeInLength = delta
				},
				attributesDidChange: { _ in return })
			let undoList = ChangeList<RSS>()
			try! rope.replace(Range(range, within: rope.units),
			    with: replacement, undoList: undoList)
			XCTAssert(changeInLength == change.changeInLength,
			    "\(changeInLength ) !~ \(change.changeInLength)")
		}
	}
}
