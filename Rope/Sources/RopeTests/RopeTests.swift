//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
import XCTest
@testable import Rope

typealias RSS = Rope<Substring>
typealias NSS = RSS.Node
typealias ECSS = RSS.ExtentController
typealias Offset = NSS.Offset

infix operator ⨯: MultiplicationPrecedence

func ⨯<L, R, Lseq : Sequence, Rseq : Sequence>(_ l: Lseq, _ r: Rseq)
    -> LazySequence<FlattenSequence<LazyMapSequence<Lseq, LazyMapSequence<Rseq, (L, R)>>>>  where Lseq.Element == L, Rseq.Element == R {
	return l.lazy.flatMap({ lelt in r.lazy.map({ relt in (lelt, relt) })})
}

class IndexOrder: XCTestCase {
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

class NestedExtentBase : XCTestCase {
	let c = [ECSS(), ECSS(), ECSS()]
	var _rope: RSS? = nil
	var rope: RSS {
		if let r = _rope {
			return r
		}
		// (abc(def(ghi)))
		// 000000000000000
		//     1111111111
		//         22222
		let r: RSS = Rope(with: .extent(under: c[0],
		                     .nodes(.text("abc"),
				     .extent(under: c[1],
				         .text("def"),
					 .extent(under: c[2], .text("ghi"))))))
		_rope = r
		return r
	}
}

class DirectSelection : XCTestCase {
	let c = [ECSS(), ECSS(), ECSS(), ECSS()]
	var _abc: RSS? = nil
	var _abcdef: RSS? = nil
	var _pqrs: RSS? = nil
	var _wxyz: RSS? = nil
	// (a)b(c)
	var abc: RSS {
		if let r = _abc {
			return r
		}
		let r: RSS = Rope(with: .nodes(.extent(under: c[0], .text("a")),
				     .text("b"),
				     .extent(under: c[1], .text("c"))))
		_abc = r
		return r
	}
	// w(x(y(z)))
	var wxyz: RSS {
		if let r = _wxyz {
			return r
		}
		let r: RSS = Rope(with: .nodes(.text("w"),
		    .extent(under: c[0], .text("x"),
		            .extent(under: c[1], .text("y"),
			            .extent(under: c[2], .text("z"))))))
		_wxyz = r
		return r
	}
	// (()(a)b(cd)ef)
	var abcdef: RSS {
		if let r = _abcdef {
			return r
		}
		let r: RSS = Rope(with:
		    .extent(under: c[0],
		        .extent(under: c[1], .empty),
		        .extent(under: c[2], .text("a")),
			.text("b"),
		        .extent(under: c[3], .text("cd")),
			.text("ef")))
		_abcdef = r
		return r
	}
	// (p(q)r)(s)
	var pqrs: RSS {
		if let r = _pqrs {
			return r
		}
		let r: RSS = Rope(with: .nodes(.extent(under: c[0], .text("p"),
		    .extent(under: c[1], .text("q")), .text("r")),
				     .extent(under: c[2], .text("s"))))
		_pqrs = r
		return r
	}
	func testDirectedWxyz0() {
		let start = wxyz.startIndex
		let end = wxyz.index(before: wxyz.endIndex)
		// *w(x(y(z))*)
		guard let (range, narrow, wide) =
		    wxyz.directed(selection: start..<end) else {
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
		    wxyz.directed(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// w(*x(y(z))*)
		let l = wxyz.index(wxyz.startIndex, offsetBy: 2)
		let r = end
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedWxyz2() {
		let start = wxyz.index(wxyz.startIndex, offsetBy: 2)
		let end = wxyz.index(before: wxyz.endIndex)
		// w(*x(y(z))*)
		guard let (range, narrow, wide) =
		    wxyz.directed(selection: start..<end) else {
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
		let start = wxyz.index(wxyz.startIndex, offsetBy: 3)
		let end = wxyz.index(before: wxyz.endIndex)
		// w(x*(y(z))*)
		guard let (range, narrow, wide) =
		    wxyz.directed(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// w(x(*y(z)*))
		let l = wxyz.index(wxyz.startIndex, offsetBy: 4)
		let r = wxyz.index(wxyz.endIndex, offsetBy: -2)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[1])
		XCTAssert(wide == c[0])
	}
	func testDirectedWxyz4() {
		let start = wxyz.index(wxyz.startIndex, offsetBy: 4)
		let end = wxyz.index(before: wxyz.endIndex)
		// w(x(*y(z))*)
		guard let (range, narrow, wide) =
		    wxyz.directed(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// w(x(*y(z)*))
		let l = start
		let r = wxyz.index(wxyz.endIndex, offsetBy: -2)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[1])
		XCTAssert(wide == c[0])
	}
	func testDirectedWxyz5() {
		let start = wxyz.index(wxyz.startIndex, offsetBy: 5)
		let end = wxyz.index(before: wxyz.endIndex)
		// w(x(y*(z))*)
		guard let (range, narrow, wide) =
		    wxyz.directed(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// w(x(y(*z*)))
		let l = wxyz.index(wxyz.startIndex, offsetBy: 6)
		let r = wxyz.index(wxyz.endIndex, offsetBy: -3)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[2])
		XCTAssert(wide == c[0])
	}
	func testDirectedWxyz6() {
		let start = wxyz.index(wxyz.startIndex, offsetBy: 6)
		let end = wxyz.index(wxyz.endIndex, offsetBy: -4)
		// w(x(y(**z)))
		guard let (range, narrow, wide) =
		    wxyz.directed(selection: start..<end) else {
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
		    abc.directed(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// *(a)b(c)*
		XCTAssert(range == abc.startIndex..<abc.endIndex)
		XCTAssert(narrow == nil)
		XCTAssert(wide == nil)
	}
	func testTightenPqrs1() {
		let start = pqrs.index(pqrs.startIndex, offsetBy: 2)
		let end = pqrs.index(pqrs.endIndex, offsetBy: -3)
		// (p*(q)r)*(s)
		guard let (range, lctlrs, rctlrs) =
		    pqrs.tightened(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (p*(q)r*)(s)
		let l = start
		let r = pqrs.index(pqrs.endIndex, offsetBy: -4)
		XCTAssert(range == l..<r)
		XCTAssert(lctlrs[...] == c[..<1])
		XCTAssert(rctlrs[...] == c[..<1])
	}
	func testDirectedPqrs1() {
		let start = pqrs.index(pqrs.startIndex, offsetBy: 2)
		let end = pqrs.index(pqrs.endIndex, offsetBy: -3)
		// (p*(q)r)*(s)
		guard let (range, narrow, wide) =
		    pqrs.directed(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (p*(q)r*)(s)
		let l = start
		let r = pqrs.index(pqrs.endIndex, offsetBy: -4)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedPqrs2() {
		let start = pqrs.index(pqrs.startIndex, offsetBy: 2)
		let end = pqrs.index(pqrs.endIndex, offsetBy: -5)
		// (p*(q)*r)(s)
		guard let (range, narrow, wide) =
		    pqrs.directed(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (p(*q*)r)(s)
		let l = pqrs.index(pqrs.startIndex, offsetBy: 3)
		let r = pqrs.index(pqrs.endIndex, offsetBy: -6)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[1])
		XCTAssert(wide == c[0])
	}
	func testDirectedAbcdef1() {
		let start = abcdef.index(abcdef.startIndex, offsetBy: 3)
		let end = abcdef.index(abcdef.endIndex, offsetBy: -5)
		// (()*(a)b(c*d)ef)
		guard let (range, narrow, wide) =
		    abcdef.directed(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (()*(a)b(c*d)ef)
		XCTAssert(range == start..<end)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedAbcdef2() {
		let start = abcdef.index(abcdef.startIndex, offsetBy: 4)
		let end = abcdef.index(abcdef.endIndex, offsetBy: -5)
		// (()(*a)b(c*d)ef)
		guard let (range, narrow, wide) =
		    abcdef.directed(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (()*(a)b(c*d)ef)
		let l = abcdef.index(abcdef.startIndex, offsetBy: 3)
		let r = end
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedAbcdef3() {
		let start = abcdef.index(abcdef.startIndex, offsetBy: 5)
		let end = abcdef.index(abcdef.endIndex, offsetBy: -5)
		// (()(a*)b(c*d)ef)
		guard let (range, narrow, wide) =
		    abcdef.directed(selection: start..<end) else {
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
		let start = abcdef.index(abcdef.startIndex, offsetBy: 2)
		let end = abcdef.index(abcdef.endIndex, offsetBy: -5)
		// ((*)(a)b(c*d)ef)
		guard let (range, narrow, wide) =
		    abcdef.directed(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (*()(a)b(c*d)ef)
		let l = abcdef.index(abcdef.startIndex, offsetBy: 1)
		let r = end
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
	func testDirectedAbcdef5() {
		let start = abcdef.index(abcdef.startIndex, offsetBy: 2)
		let end = abcdef.index(abcdef.endIndex, offsetBy: -4)
		// ((*)(a)b(cd*)ef)
		guard let (range, narrow, wide) =
		    abcdef.directed(selection: start..<end) else {
			XCTAssert(false, "\(start..<end) not found")
			return
		}
		// (*()(a)b(cd)*ef)
		let l = abcdef.index(abcdef.startIndex, offsetBy: 1)
		let r = abcdef.index(abcdef.endIndex, offsetBy: -3)
		XCTAssert(range == l..<r)
		XCTAssert(narrow == c[0])
		XCTAssert(wide == c[0])
	}
}

class IndexOffsetBy : XCTestCase {
	let c = [ECSS(), ECSS(), ECSS(), ECSS()]
	var _abcdef: RSS? = nil
	var abcdef: RSS {
		if let r = _abcdef {
			return r
		}
		// (()(a)b(cd)ef)
		let r: RSS = Rope(with:
		    .extent(under: c[0],
		        .extent(under: c[1], .empty),
		        .extent(under: c[2], .text("a")),
			.text("b"),
		        .extent(under: c[3], .text("cd")),
			.text("ef")))
		_abcdef = r
		return r
	}
	func testStart() {
		XCTAssert(abcdef.index(abcdef.startIndex, offsetBy: 0) ==
		          abcdef.startIndex)
	}
	func testEnd() {
		XCTAssert(abcdef.index(abcdef.endIndex, offsetBy: 0) ==
		          abcdef.endIndex)
	}
	func testStartToEnd() {
		XCTAssert(abcdef.index(abcdef.startIndex, offsetBy: 14) ==
		          abcdef.endIndex)
	}
	func testEndToStart() {
		XCTAssert(abcdef.index(abcdef.endIndex, offsetBy: -14) ==
		          abcdef.startIndex)
	}
	func testAll() {
		for i in 0...14 {
			let l = abcdef.index(abcdef.startIndex, offsetBy: i)
			let r = abcdef.index(abcdef.endIndex, offsetBy: i - 14)
			XCTAssert(l == r)
		}
	}
}

class RopeIndexedControllerPaths: NestedExtentBase {
	var _expectations: [[Handle]]? = nil
	var expectations: [[Handle]] {
		if let olde = _expectations {
			return olde
		}
		// (abc(def(ghi)))
		// 000000000000000
		//     1111111111
		//         22222
		let newe: [[Handle]] = [
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
			XCTAssert(rope.extents(enclosing: i) == expected)
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
	let c = [ECSS(), ECSS(), ECSS()]
	var _tree: NSS? = nil
	var tree: NSS {
		if let t = _tree {
			return t
		}
		let t: NSS = .extent(under: c[0],
		                     .nodes(.text("abc"),
				     .extent(under: c[1],
				         .text("def"),
					 .extent(under: c[2], .text("ghi")))))
		_tree = t
		return t
	}
	var _expectations: [[Handle]]? = nil
	var expectations: [[Handle]] {
		if let olde = _expectations {
			return olde
		}
		let newe: [[Handle]] = [[c[0]],
					[c[0]],
					[c[0]],
					[c[0], c[1]],
					[c[0], c[1]],
					[c[0], c[1]],
					[c[0], c[1], c[2]],
					[c[0], c[1], c[2]],
					[c[0], c[1], c[2]],
					[c[0], c[1], c[2]]]
		_expectations = newe
		return newe
	}
	let indices: [Offset] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].map { i in
	    Offset(of: i)
	}
	func testControllerPaths() {
		for (i, expected) in zip(indices, expectations) {
			XCTAssert(tree.extents(enclosing: i) == expected)
		}
	}
}

class WholeRangeUsingRopeIndices: XCTestCase {
	let ctlr = ECSS()
	var _expectations: [NSS]? = nil
	var _r: RSS? = nil
	var r: RSS {
		get {
			if let oldr = _r {
				return oldr
			}
			let newr: RSS = Rope(with: 
			    .nodes(.extent(under: ctlr, .text("abc")),
			           .text("def")))
			_r = newr
			return newr
		}
	}
	func testLookupByRange() {
		XCTAssert(r[r.startIndex..<r.endIndex] ~ r.node)
	}
}

class ExtentsOpeningClosing : XCTestCase {
	let simpleCtlr = ECSS()
	let empty: RSS = Rope()
	var _simple: RSS? = nil
	var _cplx: RSS? = nil
	var simple: RSS {
		if let old = _simple {
			return old
		}
		let r: RSS = Rope()
		r.node = .extent(under: simpleCtlr, .empty)
		_simple = r
		return r
	}
	let c = [ECSS(),	// 0
	         ECSS(),	// 1
	         ECSS(),	// 2
	         ECSS(),	// 3
	         ECSS(),	// 4
	         ECSS(),	// 5
	         ECSS(),	// 6
	         ECSS(),	// 7
	         ECSS(),	// 8
	         ECSS()]	// 9
	var cplx: RSS {
		if let old = _cplx {
			return old
		}
		// ()(a(b)c())(((def)))(((g)h)i)
		// 00111111111444444444777777777
		//     222 33  5555555  888888
		//              66666    999
		let r: RSS = Rope(with:
                    .nodes(.extent(under: c[0], .empty),
                           .extent(under: c[1],
                               .text("a"),
                               .extent(under: c[2], .text("b")),
                               .text("c"),
                               .extent(under: c[3], .empty)),
                           .extent(under: c[4],
                               .extent(under: c[5],
                                   .extent(under: c[6],
                                       .text("def")))),
                           .extent(under: c[7],
                               .extent(under: c[8],
                                   .extent(under: c[9], .text("g")),
                                   .text("h")),
                               .text("i"))))
		_cplx = r
		return r
	}
	func testClosingEmpty() {
		XCTAssert(empty.extentsClosing(at: empty.startIndex) == [])
		XCTAssert(empty.extentsClosing(at: empty.endIndex) == [])
	}
	func testOpeningEmpty() {
		XCTAssert(empty.extentsOpening(at: empty.startIndex) == [])
		XCTAssert(empty.extentsOpening(at: empty.endIndex) == [])
	}
	func testClosingSimple() {
		let middle = simple.index(after: simple.startIndex)
		XCTAssert(simple.extentsClosing(at: simple.startIndex) == [])
		XCTAssert(simple.extentsClosing(at: middle) == [simpleCtlr])
		XCTAssert(simple.extentsClosing(at: simple.endIndex) == [])
	}
	func testOpeningSimple() {
		let middle = simple.index(after: simple.startIndex)
		XCTAssert(simple.extentsOpening(at: simple.startIndex) == [])
		XCTAssert(simple.extentsOpening(at: middle) == [simpleCtlr])
		XCTAssert(simple.extentsClosing(at: simple.endIndex) == [])
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
			guard let found = cplx.extentsClosing(at: idx) else {
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
			guard let found = cplx.extentsOpening(at: idx) else {
				XCTFail("no such index")
				continue
			}
			XCTAssert(found == expected.map { i in c[i] },
			          "found \(found) expected \(expected)")
		}
	}
}

class EmptyishRopeIndices : XCTestCase {
	let one: RSS = Rope(with: .extent(under: ECSS(), .empty))
	let two: RSS = Rope(with:
	    .nodes(.extent(under: ECSS(), .empty),
		   .extent(under: ECSS(), .empty)))
	let empty: RSS = Rope()
	func testStartIndexEmpty() {
		XCTAssert(empty.startIndex == .end(of: empty))
	}
	func testEndIndexEmpty() {
		XCTAssert(empty.endIndex == .end(of: empty))
	}
	func testStartIndexOneEmptyExtent() {
		XCTAssert(one.startIndex == .start(of: one))
		XCTAssert(one.startIndex != one.endIndex)
		XCTAssert(one.index(after: one.startIndex) != one.endIndex)
		XCTAssert(one.index(one.startIndex, offsetBy: 2) ==
		    one.endIndex)
	}
	func testEndIndexOneEmptyExtent() {
		XCTAssert(one.endIndex == .end(of: one))
		XCTAssert(one.index(before: one.endIndex) != one.startIndex)
		XCTAssert(one.index(one.endIndex, offsetBy: -2) ==
		    one.startIndex)
	}
	func testStartIndexTwoEmptyExtents() {
		XCTAssert(two.startIndex == .start(of: two))
		XCTAssert(two.startIndex != two.endIndex)
		XCTAssert(two.index(after: two.startIndex) != two.endIndex)
		XCTAssert(two.index(two.startIndex, offsetBy: 2) !=
		    two.endIndex)
		XCTAssert(two.index(two.startIndex, offsetBy: 3) !=
		    two.endIndex)
		XCTAssert(two.index(two.startIndex, offsetBy: 4) ==
		    two.endIndex)
	}
	func testEndIndexTwoEmptyExtents() {
		XCTAssert(two.endIndex == .end(of: two))
		XCTAssert(two.index(before: two.endIndex) != two.startIndex)
		XCTAssert(two.index(two.endIndex, offsetBy: -2) !=
		    two.startIndex)
		XCTAssert(two.index(two.endIndex, offsetBy: -3) !=
		    two.startIndex)
		XCTAssert(two.index(two.endIndex, offsetBy: -4) ==
		    two.startIndex)
	}
}

class ThreeUnitRangesUsingRopeIndices: XCTestCase {
	let ctlr = ECSS()
	var _expectations: [NSS]? = nil
	var _r: RSS? = nil
	var expectations: [NSS] {
		get {
			if let olde = _expectations {
				return olde
			}
			let newe: [NSS] = [
			    .extent(under: ctlr, .text("ab")),
			    .extent(under: ctlr, .text("abc")),
			    .extent(under: ctlr, .text("bc")),
			    .nodes(.extent(under: ctlr, .text("c")),
			         .text("d")),
			    .nodes(.extent(under: ctlr, .empty), .text("de")),
			    .text("def")]
			_expectations = newe
			return newe
		}
	}
	var r: RSS {
		get {
			if let oldr = _r {
				return oldr
			}
			let newr: RSS = Rope(with:
			    .nodes(.extent(under: ctlr, .text("abc")),
			           .text("def")))
			_r = newr
			return newr
		}
	}
	func testLookupByRangesForward() {
		var prev = r.startIndex
		for (idx, expected) in zip(r.indices.dropFirst(3),
			                   expectations) {
			let found = r[prev..<idx]
			prev = r.index(after: prev)
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
	func testLookupByRangesBackward() {
		var prev = r.endIndex
		for (idx, expected) in zip(r.indices.reversed().dropFirst(2),
			                   expectations.reversed()) {
			let found = r[idx..<prev]
			prev = r.index(before: prev)
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
}

class TwoUnitRangesUsingRopeIndices: XCTestCase {
	let ctlr = ECSS()
	var _expectations: [NSS]? = nil
	var _r: RSS? = nil
	var expectations: [NSS] {
		get {
			if let olde = _expectations {
				return olde
			}
			let newe: [NSS] = [
			    .extent(under: ctlr, .text("a")),
			    .extent(under: ctlr, .text("ab")),
			    .extent(under: ctlr, .text("bc")),
			    .extent(under: ctlr, .text("c")),
			    .nodes(.extent(under: ctlr, .empty),
			           .text("d")),
			    .text("de"),
			    .text("ef")]
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
			newr.node = .nodes(.extent(under: ctlr, .text("abc")),
			                 .text("def"))
			_r = newr
			return newr
		}
	}
	func testLookupByRangesForward() {
		var prev = r.startIndex
		for (idx, expected) in zip(r.indices.dropFirst(2),
			                   expectations) {
			let found = r[prev..<idx]
			prev = r.index(after: prev)
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
	func testLookupByRangesBackward() {
		var prev = r.endIndex
		for (idx, expected) in zip(r.indices.reversed().dropFirst(1),
			                   expectations.reversed()) {
			let found = r[idx..<prev]
			prev = r.index(before: prev)
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
}

class UnitRangesUsingRopeIndices: XCTestCase {
	let ctlr = ECSS()
	var _expectations: [NSS]? = nil
	var _r: RSS? = nil
	var expectations: [NSS] {
		get {
			if let olde = _expectations {
				return olde
			}
			let newe: [NSS] = [.empty,
			    .extent(under: ctlr, .empty),
			    .extent(under: ctlr, .text("a")),
			    .extent(under: ctlr, .text("b")),
			    .extent(under: ctlr, .text("c")),
			    .extent(under: ctlr, .empty),
			    .text("d"),
			    .text("e"),
			    .text("f")]
			_expectations = newe
			return newe
		}
	}
	var r: RSS {
		get {
			if let oldr = _r {
				return oldr
			}
			let newr: RSS = Rope(with:
			    .nodes(.extent(under: ctlr, .text("abc")),
			           .text("def")))
			_r = newr
			return newr
		}
	}
	func testLookupByRangesForward() {
		var prev = r.startIndex
		for (idx, expected) in zip(r.indices, expectations) {
			let found = r[prev..<idx]
			prev = idx
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
	func testLookupByRangesBackward() {
		var prev = r.endIndex
		for (idx, expected) in zip(r.indices.reversed(),
                                           expectations.reversed()) {
			let found = r[idx..<prev]
			prev = idx
			XCTAssert(found ~ expected,
			          "found \(found) expected \(expected)")
		}
	}
}

class LookupUsingRopeIndicesDerivedFromUTF16Offsets: XCTestCase {
	let ctlr = ECSS()
	var _expectations: [NSS]? = nil
	var _r: RSS? = nil
	var expectations: [NSS] {
		get {
			if let olde = _expectations {
				return olde
			}
			let newe: [NSS] = [
			    .extent(under: ctlr, .text("a")),
			    .extent(under: ctlr, .text("b")),
			    .extent(under: ctlr, .text("c")),
			    .text("d"),
			    .text("e"),
			    .text("f")]
			_expectations = newe
			return newe
		}
	}
	var r: RSS {
		get {
			if let oldr = _r {
				return oldr
			}
			let newr: RSS = Rope(with:
			    .nodes(.extent(under: ctlr, .text("abc")),
			           .text("def")))
			_r = newr
			return newr
		}
	}
	func testIterateElements() {
		for (i, expected) in expectations.enumerated() {
			let ofs = Offset(of: i)
			let idx = RSS.Index(utf16Offset: ofs, in: r)
			let found = r[idx]
			XCTAssert(found == expected,
			    "found \(found) expected \(expected)")
		}
	}
	func testEndIndex() {
		let ofs = Offset(of: expectations.count)
		let idx = RSS.Index(utf16Offset: ofs, in: r)
		XCTAssertThrowsError(try r.element(at: idx))
	}
}

class ExtentElementLookupUsingRopeIndices: XCTestCase {
	let ctlr = ECSS()
	var _expectations: [NSS]? = nil
	var _r: RSS? = nil
	var expectations: [NSS] {
		get {
			if let olde = _expectations {
				return olde
			}
			let newe: [NSS] = [.empty,
			    .extent(under: ctlr, .text("a")),
			    .extent(under: ctlr, .text("b")),
			    .extent(under: ctlr, .text("c")),
			    .extent(under: ctlr, .empty),
			    .text("d"),
			    .text("e"),
			    .text("f")]
			_expectations = newe
			return newe
		}
	}
	var r: RSS {
		get {
			if let oldr = _r {
				return oldr
			}
			let newr: RSS = Rope(with:
			    .nodes(.extent(under: ctlr, .text("abc")),
			           .text("def")))
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
		XCTAssertThrowsError(try r.element(at: idx))
	}
}

class BasicElementLookupUsingRopeIndex: XCTestCase {
	let rope1: RSS = Rope(content: "abc")
	let rope2: RSS = Rope(content: "def")
	func testStartIndex() {
		let idx1 = rope1.startIndex
		let idx2 = rope2.startIndex
		XCTAssert(rope1[idx1].content == "a")
		XCTAssert(rope2[idx2].content == "d")
	}
	func testSecondIndex() {
		let idx1 = rope1.index(after: rope1.startIndex)
		let idx2 = rope2.index(after: rope2.startIndex)
		XCTAssert(rope1[idx1].content == "b")
		XCTAssert(rope2[idx2].content == "e")
	}
	func testThirdIndex() {
		let idx1 = rope1.index(after:
		           rope1.index(after: rope1.startIndex))
		let idx2 = rope2.index(after:
		           rope2.index(after: rope2.startIndex))
		XCTAssert(rope1[idx1].content == "c")
		XCTAssert(rope2[idx2].content == "f")
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
		XCTAssert(rope1[idx1].content == "a")
		XCTAssert(rope2[idx2].content == "d")
		XCTAssert(idx1 == rope1.startIndex)
		XCTAssert(idx2 == rope2.startIndex)
	}
	func testTwoLeftOfEndIndex() {
		let idx1 = rope1.index(before:
		           rope1.index(before: rope1.endIndex))
		let idx2 = rope2.index(before:
		           rope2.index(before: rope2.endIndex))
		XCTAssert(rope1[idx1].content == "b")
		XCTAssert(rope2[idx2].content == "e")
	}
	func testOneLeftOfEndIndex() {
		let idx1 = rope1.index(before: rope1.endIndex)
		let idx2 = rope2.index(before: rope2.endIndex)
		XCTAssert(rope1[idx1].content == "c")
		XCTAssert(rope2[idx2].content == "f")
		XCTAssert(rope1.index(after: idx1) == rope1.endIndex)
		XCTAssert(rope2.index(after: idx2) == rope2.endIndex)
	}
	func testEndIndex() {
		let idx1 = rope1.endIndex
		let idx2 = rope2.endIndex
		XCTAssertThrowsError(try rope1.element(at: idx1))
		XCTAssertThrowsError(try rope2.element(at: idx2))
	}
}

class CompareDisparateRopeIndices: XCTestCase {
	let rope1: RSS = Rope(content: "abc")
	let rope2: RSS = Rope(content: "def")
	func testStartIndices() {
		let idx1 = rope1.startIndex
		let idx2 = rope2.startIndex
		XCTAssertThrowsError(try idx1.isLessThan(idx2))
		XCTAssertThrowsError(try idx1.equals(idx2))
	}
	func testSecondIndices() {
		let idx1 = rope1.index(after: rope1.startIndex)
		let idx2 = rope2.index(after: rope2.startIndex)
		XCTAssertThrowsError(try idx1.isLessThan(idx2))
		XCTAssertThrowsError(try idx1.equals(idx2))
	}
	func testEndIndices() {
		let idx1 = rope1.endIndex
		let idx2 = rope2.endIndex
		XCTAssertThrowsError(try idx1.isLessThan(idx2))
		XCTAssertThrowsError(try idx1.equals(idx2))
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
		let first: NSS = .text("abc")
		let handle = Handle()
		guard case .step(let second) =
		    first.inserting(handle, after: .rightStep) else {
			XCTFail("inserting(_,after:) failed")
			return
		}
		XCTAssert(second.leaves.map { (x: RSS.Node) -> Bool in
			if case .index(let w) = x {
				return w.get() == handle
			} else {
				return false
			}
		}[1])
	}

	func testStepAndReleaseIndex() {
		let first: NSS = .text("abc")
		var handle = Handle()
		guard case .step(let second) =
		    first.inserting(handle, after: .rightStep) else {
			XCTFail("inserting(_,after:) failed")
			return
		}
		handle = Handle()
		XCTAssert(!second.leaves.map { (x: RSS.Node) -> Bool in
			if case .index(let w) = x {
				return w.get() == handle
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
		    HandleHolding.isIndex).count == 11)
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
		    HandleHolding.isIndex).count == 0)
	}

	func testReleasingIndices() {
		let rope: RSS = Rope(content: "abcdefghijkl")
		var indices: [RSS.Index]? = []

		for i in rope.indices {
			indices?.append(i)
		}

		indices = nil
		print(rope.node.leaves)
		print(rope.node.leaves.filter(HandleHolding.isNilIndex))

		XCTAssert(rope.node.leaves.filter(
		    HandleHolding.isNilIndex).count == 11)
	}

	func testPerformanceExample() {
		// This is an example of a performance test case.
		self.measure {
			// Put the code you want to measure the time of here.
		}
	}
}

class NodeSubropes : XCTestCase {
	let n: NSS = .nodes(.text("abc"), .text("defgh"), .text("ijkl"))

	func testFullContent() {
		XCTAssert(n.content == "abcdefghijkl")
	}

	func testLeadingSubnode() {
		let section = n.subrope(from: 0, upTo: Offset(of: 3))
		XCTAssert(section.content == "abc")
	}

	func testCrossingFirstTwoSubnodes() {
		let section = n.subrope(from: 0, upTo: Offset(of: 5))
		XCTAssert(section.content == "abcde")
	}

	func testSecondSubnode() {
		let section = n.subrope(from: Offset(of: 3),
                                        upTo: Offset(of: 8))
		XCTAssert(section.content == "defgh")
	}

	func testTrailingTwoSubnodes() {
		let section = n.subrope(from: Offset(of: 3),
		                        upTo: Offset(of: 12))
		XCTAssert(section.content == "defghijkl")
	}

	func testCrossingLastTwoSubnodes() {
		let section = n.subrope(from: Offset(of: 4),
		                        upTo: Offset(of: 9))
		XCTAssert(section.content == "efghi")
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
	let n: NSS = .nodes(abc, defgh, ijkl)

	func testFrontAttributes() {
		let (attrs, range) = n.attributes(at: 0)
		XCTAssert(Self.frontAttrs ~ attrs)
		XCTAssert(range == Offset.utf16RangeTo(3))
	}
	func testMiddleAttributes() {
		let (attrs, range) = n.attributes(at: Offset(of: 3))
		XCTAssert(Self.middleAttrs ~ attrs)
		XCTAssert(range == Offset.utf16Range(3..<8))
	}
	func testBackAttributes() {
		let (attrs, range) = n.attributes(at: Offset(of: 8))
		XCTAssert(Self.backAttrs ~ attrs)
		XCTAssert(range == Offset.utf16Range(8..<12))
	}
	func testLastAttributes() {
		let (attrs, range) = n.attributes(at: Offset(of: 11))
		XCTAssert(Self.backAttrs ~ attrs)
		XCTAssert(range == Offset.utf16Range(8..<12))
	}
	func testSettingFrontAndMiddleAttributes() {
		let newn = n.settingAttributes(NodeAttributes.newAttrs,
		    range: Offset.utf16Range(0..<8))
		let (attrs, frontRange) = newn.attributes(at: 0)
		XCTAssert(Self.newAttrs ~ attrs)
		XCTAssert(frontRange == Offset.utf16Range(0..<3))
		let (_, middleRange) =
		    newn.attributes(at: Offset(of: 3))
		XCTAssert(middleRange == Offset.utf16Range(3..<8))
	}
	static func helpTestSettingCentralAttributes(_ oldn: NSS) {
		let newn = oldn.settingAttributes(NodeAttributes.newAttrs,
		    range: Offset.utf16Range(2..<9))

		let (frontAttrs, frontRange) = newn.attributes(at: 0)
		XCTAssert(frontRange == Offset.utf16Range(0..<2))
		XCTAssert(Self.frontAttrs ~ frontAttrs)

		let (midAttrs1, midRange1) =
		    newn.attributes(at: Offset(of: 2))
		XCTAssert(midRange1 == Offset.utf16Range(2..<3))
		XCTAssert(Self.newAttrs ~ midAttrs1)

		let (midAttrs2, midRange2) =
		    newn.attributes(at: Offset(of: 3))
		XCTAssert(midRange2 == Offset.utf16Range(3..<8))
		XCTAssert(Self.newAttrs ~ midAttrs2)

		let (midAttrs3, midRange3) =
		    newn.attributes(at: Offset(of: 8))
		XCTAssert(midRange3 == Offset.utf16Range(8..<9))
		XCTAssert(Self.newAttrs ~ midAttrs3)

		let (backAttrs, backRange) =
		    newn.attributes(at: Offset(of: 9))
		XCTAssert(backRange == Offset.utf16Range(9..<12))
		XCTAssert(Self.backAttrs ~ backAttrs)
	}
	func testSettingCentralAttributes() {
		NodeAttributes.helpTestSettingCentralAttributes(n)
	}
/*
	func testSettingCentralAttributesWithCursor() {
		let ctlr = ECSS()
		let contn: NSS = .extent(under: ctlr, n)
		NodeAttributes.helpTestSettingCentralAttributes(contn)
	}
*/
	func testSettingCentralAttributesWithExtent() {
		let ctlr = ECSS()
		let contn: NSS = .extent(under: ctlr, n)
		NodeAttributes.helpTestSettingCentralAttributes(contn)
	}
	func testSettingBackAttributes() {
		let newn = n.settingAttributes(NodeAttributes.newAttrs,
		    range: Offset.utf16Range(8..<12))
		let (attrs, range) = newn.attributes(at: Offset(of: 8))
		XCTAssert(Self.newAttrs ~ attrs)
		XCTAssert(range == Offset.utf16Range(8..<12),
		    "actual range \(range) newn \(newn.debugDescription) " +
		    "n \(n.debugDescription)")
	}
	func testSettingLastAttributes() {
		let newn = n.settingAttributes(NodeAttributes.newAttrs,
		    range: Offset.utf16Range(11..<12))
		let (attrs, range) = newn.attributes(at: Offset(of: 11))
		XCTAssert(Self.newAttrs ~ attrs)
		XCTAssert(range == Offset.utf16Range(11..<12))
		let (_, abuttingRange) = newn.attributes(at: Offset(of: 8))
		XCTAssert(abuttingRange == Offset.utf16Range(8..<11))
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

class HandleSets : XCTestCase {
	func testInit() {
		let set = HandleSet()
		XCTAssert(set.cursorCount == 0)
		XCTAssert(set.extentCount == 0)
		XCTAssert(set.indexCount == 0)
	}
	func testSeqInit() {
		let ids: [Handle.Id] = [.cursor(0), .extent(1), .index(2)]
		let set = HandleSet(ids)
		XCTAssert(set.cursorCount == 1)
		XCTAssert(set.extentCount == 1)
		XCTAssert(set.indexCount == 1)
	}
	func testLiteralInit() {
		let set: [HandleSet] = [[.cursor(0), .extent(0), .index(0)],
		                         [.cursor(0), .extent(0), .index(0),
					  .index(1)],
		                         [.cursor(0), .cursor(1), .extent(0),
					  .index(0)],
		                         [.cursor(0), .extent(0), .extent(1),
					  .index(0)]]
		XCTAssert(set[0].cursorCount == 1)
		XCTAssert(set[0].extentCount == 1)
		XCTAssert(set[0].indexCount == 1)
		XCTAssert(set[1].cursorCount == 1)
		XCTAssert(set[1].extentCount == 1)
		XCTAssert(set[1].indexCount == 2)
		XCTAssert(set[2].cursorCount == 2)
		XCTAssert(set[2].extentCount == 1)
		XCTAssert(set[2].indexCount == 1)
		XCTAssert(set[3].cursorCount == 1)
		XCTAssert(set[3].extentCount == 2)
		XCTAssert(set[3].indexCount == 1)
	}
	func testUnion() {
		let set: [HandleSet] = [[.cursor(0), .extent(0), .index(0)],
		                         [.cursor(0), .extent(0), .index(0),
					  .index(1)],
		                         [.cursor(0), .cursor(1), .extent(0),
					  .index(0)],
		                         [.cursor(0), .extent(0), .extent(1),
					  .index(0)]]
		XCTAssert(set[0].union(set[1]).cursorCount == 1)
		XCTAssert(set[0].union(set[1]).extentCount == 1)
		XCTAssert(set[0].union(set[1]).indexCount == 2)
		XCTAssert(set[1].union(set[2]).cursorCount == 2)
		XCTAssert(set[1].union(set[2]).extentCount == 1)
		XCTAssert(set[1].union(set[2]).indexCount == 2)
		XCTAssert(set[2].union(set[3]).cursorCount == 2)
		XCTAssert(set[2].union(set[3]).extentCount == 2)
		XCTAssert(set[2].union(set[3]).indexCount == 1)
		var p = set[0], q = set[1]
		p.formUnion(q)
		XCTAssert(p.cursorCount == 1)
		XCTAssert(p.extentCount == 1)
		XCTAssert(p.indexCount == 2)
	}
	func testIntersection() {
		let overlap: [HandleSet] = [[.cursor(0), .extent(0), .index(0)],
		                         [.cursor(0), .extent(0), .index(0),
					  .index(1)],
		                         [.cursor(0), .cursor(1), .extent(0),
					  .index(0)],
		                         [.cursor(0), .extent(0), .extent(1),
					  .index(0)]]

		XCTAssert(overlap[0].intersection(overlap[1]).cursorCount == 1)
		XCTAssert(overlap[0].intersection(overlap[1]).extentCount == 1)
		XCTAssert(overlap[0].intersection(overlap[1]).indexCount == 1)
		XCTAssert(overlap[1].intersection(overlap[2]).cursorCount == 1)
		XCTAssert(overlap[1].intersection(overlap[2]).extentCount == 1)
		XCTAssert(overlap[1].intersection(overlap[2]).indexCount == 1)
		XCTAssert(overlap[2].intersection(overlap[3]).cursorCount == 1)
		XCTAssert(overlap[2].intersection(overlap[3]).extentCount == 1)
		XCTAssert(overlap[2].intersection(overlap[3]).indexCount == 1)

		let disj: [HandleSet] = [[.cursor(0), .extent(0), .index(0)],
		                         [.cursor(4), .extent(4), .index(4),
					  .index(8)],
		                         [.cursor(8), .cursor(12), .extent(8),
					  .index(12)],
		                         [.cursor(16), .extent(12), .extent(16),
					  .index(16)]]

		XCTAssert(disj[0].intersection(disj[1]).cursorCount == 0)
		XCTAssert(disj[0].intersection(disj[1]).extentCount == 0)
		XCTAssert(disj[0].intersection(disj[1]).indexCount == 0)
		XCTAssert(disj[1].intersection(disj[2]).cursorCount == 0)
		XCTAssert(disj[1].intersection(disj[2]).extentCount == 0)
		XCTAssert(disj[1].intersection(disj[2]).indexCount == 0)
		XCTAssert(disj[2].intersection(disj[3]).cursorCount == 0)
		XCTAssert(disj[2].intersection(disj[3]).extentCount == 0)
		XCTAssert(disj[2].intersection(disj[3]).indexCount == 0)
	}
	func testSymmetricDifference() {
		let overlap: [HandleSet] = [
		    [.cursor(0), .extent(0), .index(0)],		// 0
		    [.cursor(0), .extent(0), .index(0), .index(1)],	// 1
		    [.cursor(0), .cursor(1), .extent(0), .index(0)],	// 2
		    [.cursor(0), .extent(0), .extent(1), .index(0)]]	// 3

		let diff0 = overlap[0].symmetricDifference(overlap[1])
		XCTAssert(diff0.cursorCount == 0)
		XCTAssert(diff0.extentCount == 0)
		XCTAssert(diff0.indexCount == 1)

		let diff1 = overlap[1].symmetricDifference(overlap[2])
		XCTAssert(diff1.cursorCount == 1)
		XCTAssert(diff1.extentCount == 0)
		XCTAssert(diff1.indexCount == 1)

		let diff2 = overlap[2].symmetricDifference(overlap[3])
		XCTAssert(diff2.cursorCount == 1)
		XCTAssert(diff2.extentCount == 1)
		XCTAssert(diff2.indexCount == 0)

		let disj: [HandleSet] = [
		    [.cursor(0), .extent(0), .index(0)],		// 0
		    [.cursor(4), .extent(4), .index(4), .index(8)],	// 1
		    [.cursor(8), .cursor(12), .extent(8), .index(12)],	// 2
		    [.cursor(16), .extent(12), .extent(16), .index(16)]]// 3

		let diff3 = disj[0].symmetricDifference(disj[1])

		XCTAssert(diff3.cursorCount == 2)
		XCTAssert(diff3.extentCount == 2)
		XCTAssert(diff3.indexCount == 3)

		let diff4 = disj[1].symmetricDifference(disj[2])

		XCTAssert(diff4.cursorCount == 3)
		XCTAssert(diff4.extentCount == 2)
		XCTAssert(diff4.indexCount == 3)

		let diff5 = disj[2].symmetricDifference(disj[3])

		XCTAssert(diff5.cursorCount == 3)
		XCTAssert(diff5.extentCount == 3)
		XCTAssert(diff5.indexCount == 2)
	}
	func testInsert() {
		var set: HandleSet = [.cursor(0)]

		XCTAssert(set.cursorCount == 1)
		XCTAssert(set.insert(.cursor(0)) == (false, .cursor(0)))
		XCTAssert(set.cursorCount == 1)
		XCTAssert(set.insert(.cursor(1)) == (true, .cursor(1)))
		XCTAssert(set.cursorCount == 2)
		XCTAssert(set.extentCount == 0)
		XCTAssert(set.indexCount == 0)
	}
	func testRemove() {
		var set: HandleSet = [.cursor(0)]

		XCTAssert(set.cursorCount == 1)
		XCTAssert(set.remove(.cursor(1)) == nil)
		XCTAssert(set.cursorCount == 1)
		XCTAssert(set.remove(.cursor(0)) == .cursor(0))
		XCTAssert(set.cursorCount == 0)
		XCTAssert(set.extentCount == 0)
		XCTAssert(set.indexCount == 0)
	}
	func testUpdate() {
		var set: HandleSet = [.cursor(0)]

		XCTAssert(set.cursorCount == 1)
		XCTAssert(set.update(with: .cursor(0)) == .cursor(0))
		XCTAssert(set.cursorCount == 1)
		XCTAssert(set.update(with: .cursor(1)) == nil)
		XCTAssert(set.cursorCount == 2)
		XCTAssert(set.extentCount == 0)
		XCTAssert(set.indexCount == 0)
	}
}

class TightenSelection: NestedExtentBase {
	public func testBothTighten1() {
		// *(abc(def(ghi)))*
		let start = rope.startIndex
		let end = rope.endIndex
		let outer = start..<end
		let result = rope.tightened(selection: outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightened(selection:)")
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
		let start = rope.index(rope.startIndex, offsetBy: 4)
		let end = rope.index(rope.endIndex, offsetBy: -1)
		let outer = start..<end
		let result = rope.tightened(selection: outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightened(selection:)")
			return
		}
		let l = rope.index(rope.startIndex, offsetBy: 5)
		let r = rope.index(rope.endIndex, offsetBy: -2)
		// (abc(*def(ghi)*))
		XCTAssert(tightened == l..<r)
		XCTAssert(lctlrs[...] == c[..<2])
		XCTAssert(rctlrs[...] == c[..<2])
	}
	public func testBothTighten3() {
		// (abc(def*(ghi)*))
		let start = rope.index(rope.startIndex, offsetBy: 8)
		let end = rope.index(rope.endIndex, offsetBy: -2)
		let outer = start..<end
		let result = rope.tightened(selection: outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightened(selection:)")
			return
		}
		let l = rope.index(rope.startIndex, offsetBy: 9)
		let r = rope.index(rope.endIndex, offsetBy: -3)
		// (abc(def(*ghi*)))
		XCTAssert(tightened == l..<r)
		XCTAssert(lctlrs == c)
		XCTAssert(rctlrs == c)
	}
	/* All cursor positions:
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
		// *(abc(def(ghi))*)
		let start = rope.startIndex
		let end = rope.index(rope.endIndex, offsetBy: -1)
		let outer = start..<end
		let result = rope.tightened(selection: outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightened(selection:)")
			return
		}
		let l = rope.index(rope.startIndex, offsetBy: 1)
		let r = rope.index(rope.endIndex, offsetBy: -1)
		// (*abc(def(ghi))*)
		XCTAssert(tightened == l..<r)
		XCTAssert(lctlrs[...] == c[..<1])
		XCTAssert(rctlrs[...] == c[..<1])
	}
	public func testRightTighten1() {
		// (*abc(def(ghi)))*
		let start = rope.index(rope.startIndex, offsetBy: 1)
		let end = rope.endIndex
		let outer = start..<end
		let result = rope.tightened(selection: outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightened(selection:)")
			return
		}
		let l = start
		let r = rope.index(rope.endIndex, offsetBy: -1)
		// (*abc(def(ghi))*)
		XCTAssert(tightened == l..<r)
		XCTAssert(lctlrs[...] == c[..<1])
		XCTAssert(rctlrs[...] == c[..<1])
	}
	public func testUntightenable1() {
		// (*abc(def(ghi))*)
		let start = rope.index(rope.startIndex, offsetBy: 1)
		let end = rope.index(rope.endIndex, offsetBy: -1)
		let outer = start..<end
		let result = rope.tightened(selection: outer)
		guard let (tightened, lctlrs, rctlrs) = result else {
			XCTAssert(false,
			    "expected non-nil .tightened(selection:)")
			return
		}
		// (*abc(def(ghi))*)
		XCTAssert(tightened == outer)
		XCTAssert(lctlrs[...] == c[..<1])
		XCTAssert(rctlrs[...] == c[..<1])
	}
}

class CompareIndicesAndEndComplicatedRopes: NestedExtentBase {
	func testEquals() {
		XCTAssert(
		    rope.index(after: rope.index(before: rope.endIndex)) ==
		    rope.endIndex)
		XCTAssert(
		    rope.index(rope.index(rope.endIndex, offsetBy: -2),
		               offsetBy: 2) == rope.endIndex)
	}
	/* Delete right of index(pqrs.endIndex, offsetBy: n < 0): an interior
	 * index is no longer interior if the content to its right was
	 * deleted.
	 */
	func testEdits() {
		let pqrs = RSS(content: "pqrs")
		let idx = pqrs.index(pqrs.endIndex, offsetBy: -2)
		let range = Offset.utf16Range(2..<4)
		/* Unfortunately, the following removes the .index(_) node
		 * corresponding to `idx`:
		 */
		pqrs[range] = Substring("")
		XCTAssert(idx == pqrs.endIndex)
	}
}

class CompareIndicesAndStartComplicatedRopes: NestedExtentBase {
	func testEquals() {
		XCTAssert(
		    rope.index(before: rope.index(after: rope.startIndex)) ==
		    rope.startIndex)
		XCTAssert(
		    rope.index(rope.index(rope.startIndex, offsetBy: 2),
		               offsetBy: -2) == rope.startIndex)
	}
	/* Delete left of index(pqrs.startIndex, offsetBy: n > 0): an interior
	 * index is no longer interior if the content to its left was
	 * deleted.
	 */
	func testEdits() {
		let pqrs = RSS(content: "pqrs")
		let idx = pqrs.index(pqrs.startIndex, offsetBy: 2)
		let range = Offset.utf16Range(0..<2)
		/* Unfortunately, the following removes the .index(_) node
		 * corresponding to `idx`:
		 */
		pqrs[range] = Substring("")
		XCTAssert(idx == pqrs.startIndex)
	}
}

class CompareDisparateIndicesComplicatedRopes: NestedExtentBase {
	/* All cursor positions:
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
		XCTAssert(rope.index(before: rope.endIndex) ==
		    rope.index(rope.endIndex, offsetBy: -1))
		XCTAssert(
		    rope.index(before: rope.index(before: rope.endIndex)) ==
		    rope.index(rope.endIndex, offsetBy: -2))
		XCTAssert(rope.index(before: rope.endIndex) ==
		    rope.index(rope.startIndex, offsetBy: 14))
		XCTAssert(rope.index(after: rope.startIndex) ==
		    rope.index(rope.endIndex, offsetBy: -14))
		let n = rope.indices.count + 1
		/* Test 3 times.  Each test leaves stale indices behind
		 * that should confound defective index comparison.
		 */
		for _ in 0..<3 {
			for step in 0..<n {
				let i1 = rope.index(rope.startIndex,
				                    offsetBy: step)
				let i2 = rope.index(rope.endIndex,
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
			let numberedIndices =
			    (rope.indices + [rope.endIndex]).enumerated()
			for ((n1, idx1), (n2, idx2)) in
			    numberedIndices ⨯ numberedIndices {
				XCTAssert((n1 == n2) && (idx1 == idx2) ||
					  (n1 < n2) && (idx1 < idx2) ||
					  (n1 > n2) && (idx1 > idx2))
			}
		}
	}
}
