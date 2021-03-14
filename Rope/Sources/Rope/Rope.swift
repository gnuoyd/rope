//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
import Foundation

enum RopeNoSuchElement : Error {
case onInterior
case atStart
case atEnd
}

extension Range {
	init<C : Content>(utf16Range r: Range<Rope<C>.Node.Offset>,
	                   in rope: Rope<C>) where Bound == Rope<C>.Index {
		let lower = Rope.Index(abutting: r.lowerBound, on: .right,
		    in: rope)
		let upper = Rope.Index(abutting: r.upperBound, on: .left,
		    in: rope)
		self = lower..<upper
	}
}

extension Rope.Node {
	public struct Dimensions {
		public let jots: Int
		public let utf16Offset: Offset
		public static var zero: Dimensions {
			return Dimensions(jots: 0, utf16Offset: 0)
		}
		public init(jots: Int = 0, utf16Offset: Offset = 0) {
			self.jots = jots
			self.utf16Offset = utf16Offset
		}
		public var halfPerimeter: Int {
			guard case .offset(let n) = utf16Offset else {
				return jots
			}
			return jots + n
		}
	}
}

extension Rope.Node.Dimensions {
	public static func +(_ l: Self, _ r: Self) -> Self {
		return Self(jots: l.jots + r.jots,
		            utf16Offset: l.utf16Offset + r.utf16Offset)
	}
}

extension Rope.Node.Offset : ExpressibleByIntegerLiteral {
	public typealias IntegerLiteralType = Int
	public init(integerLiteral n: Self.IntegerLiteralType) {
		self = .offset(n)
	}
}

extension Rope.Node.Offset {
	public init(of n: Int) {
		self = .offset(n)
	}
	public var utf16Offset: Int {
		switch self {
		case .offset(let n):
			return n
		}
	}
}

/* Use cases:
 *
 * Get/set/add/remove attributes on characters.
 *
 * Get/set/add/remove attributes on a cursor.
 *
 * Enclose a range in an extent.  The range must be
 * well-formed: must begin and end inside the same extent. 
 *
 * Insert a cursor between characters or nested between extents.
 *
 * Remove a cursor.
 *
 * "Step" a cursor left or right by a character.
 *
 * "Scoot" a cursor left or right by an extent boundary.
 *
 * "Scoot" a cursor left or right by a cursor?
 *
 * Replace a cursor by an extent; apply the cursor's attributes to
 * the extent's content.
 *
 * Insert some text left of a cursor; apply the cursor's attributes
 * to the text.
 */
public class Rope<C : Content> : Collection {
	public enum Climb {
	case `in`
	case out
	}
	public typealias Content = C
	public typealias Element = Node
	public typealias Offset = Node.Offset
	public enum Index : Comparable {
	case start(of: Rope)
	case end(of: Rope)
	case interior(of: Rope, handle: Handle)
	}
	public class ExtentController : Handle {
		public override var id: Id { return .extent(_id) }
	}

	/* A Node directly encodes the presence of cursors because it is
	 * possible for a cursor to move up and down the hierarchy of text
	 * extents without changing between-character positions.  A cursor
	 * can appear at the position left of the first character or right of
	 * the last character in a Node.  A cursor can also appear in a
	 * Node that contains no characters.
	 */
	public indirect enum Node {
	public typealias Content = C
	public typealias Element = C.Element
	public enum Offset {
		case offset(Int)
	}
	case cursor(Handle, Attributes)
	case index(Weak<Handle>)
	case extent(ExtentController, Node)
	case concat(Node, Offset, UInt, HandleSet, Node, Dimensions)
	case leaf(Attributes, C)
	case empty
	}

	private var mutations: UInt = 0
	private var rebalanceInterval: UInt = 32
	private var _top: Node
	private var top: Node {
		get {
			return _top
		}
		set {
			mutations += 1
			if mutations.isMultiple(of: rebalanceInterval) {
				_top =
				    newValue.cleaned()?.rebalanced() ?? .empty
			} else {
				_top = newValue
			}
		}
	}
	public var startIndex: Index {
		/* There are at least three index positions, start and
		 * end, if there is even a solitary extent.  Need to return
		 * .start(of: self) in that case.
		 */
		if top.startIndex == top.endIndex && top.hids.extentCount == 0 {
			return .end(of: self)
		}
		return .start(of: self)
	}
	public var endIndex: Index { return .end(of: self) }

	public init() {
		_top = .empty
	}
	public var node: Node {
		get {
			return top
		}
		set {
			top = newValue
		}
	}
	public func index(_ h1: Handle, precedes h2: Handle) -> Bool? {
		return top.index(h1, precedes: h2)
	}
	public init<T>(content t: T) where C : Initializable,
	    C.Initializer == T, T : Collection {
		_top = Node(content: t)
	}
	public func index(after i: Index) -> Index {
		guard i.owner === self else {
			fatalError("Mismatched owner")
		}
		switch i {
		case .start(_):
			let h = Handle()
			guard case .step(let n) =
			    top.inserting(h, after: .rightStep) else {
				return .end(of: self)
			}
			top = n
			return .interior(of: self, handle: h)
		case .end(_):
			fatalError("No index after .endIndex")
		case .interior(_, let h):
			let j = Handle()
			switch top.inserting(j, one: .rightStep, after: h) {
			case .inchOut:
				fatalError(".interior(_, \(h)) already at end?")
			case .absent:
				fatalError(".interior(_, \(h)) is absent")
			case .stepOut:
				return .end(of: self)
			case .step(let node):
				top = node
				return .interior(of: self, handle: j)
			}
		}
	}
	public func index(before i: Index) -> Index {
		guard i.owner === self else {
			fatalError("Mismatched owner")
		}
		switch i {
		case .start(_):
			fatalError("No index before .startIndex")
		case .end(_):
			let h = Handle()
			guard case .step(let n) =
			    top.inserting(h, after: .leftStep) else {
				return .start(of: self)
			}
			top = n
			return .interior(of: self, handle: h)
		case .interior(_, let h):
			let j = Handle()
			switch top.inserting(j, one: .leftStep, after: h) {
			case .inchOut:
				fatalError(
				    ".interior(_, \(h)) already at start?")
			case .absent:
				fatalError(".interior(_, \(h)) is absent")
			case .stepOut:
				return .start(of: self)
			case .step(let node):
				top = node
				return .interior(of: self, handle: j)
			}
		}
	}
	public subscript(i: Offset) -> Content.Element {
		return top.element(at: i)
	}
	public subscript(r: Range<Index>) -> Element {
		get {
			guard let e = top.subrope(after: r.lowerBound,
			    to: r.upperBound) else {
				fatalError("No such range")
			}
			return e
		}
	}
	public subscript(i: Index) -> Element {
		get {
			do {
				return try element(at: i)
			} catch {
				fatalError("No such element")
			}
		}
	}
	public func element(at i: Index) throws -> Element {
		switch i {
		case .start(_):
			guard case .step(let node) = top.firstElement() else {
				throw RopeNoSuchElement.atStart
			}
			return node
		case .interior(_, let h):
			let result = top.element(at: h)
			guard case .step(let node) = result else {
				throw RopeNoSuchElement.onInterior
			}
			return node
		case .end(_):
			throw RopeNoSuchElement.atEnd
		}
	}
	public func insert(_ elt: Element, at i: Index) -> Bool {
		guard self === i.owner else {
			return false
		}
		if case .empty = elt {
			return false
		}
		switch i {
		case .start(_):
			top = .nodes(elt, top)
		case .end(_):
			top = .nodes(top, elt)
		case .interior(_, let h):
			guard let newtop = top.inserting(elt, at: h) else {
				return false
			}
			top = newtop
		}
		return true
	}
	/* TBD tests */
	public subscript(_ r: Range<Offset>) -> Content {
		set(newValue) {
			let ir = Range(utf16Range: r, in: self)
			guard let newtop = top.replacing(ir,
			    with: newValue) else {
				fatalError("No such range")
			}
			top = newtop
		}
		get {
			let ir = Range(utf16Range: r, in: self)
			return top[ir]
		}
	}
/*
	public subscript<I>(_ r: Range<Offset>) -> I
	    where C : Initializable, C.Initializer == I, I : Collection,
	          I : Initializable, I.Initializer == C {
		set(newValue) {
			top = top.replacing(r, with: C(newValue))
		}
		get {
			return I(top[r])
		}
	}
*/
	public func attributes(at i: Offset) -> (Attributes, Range<Offset>) {
		return top.attributes(at: i)
	}
	public func setAttributes(_ attrs: Attributes, range r: Range<Offset>){
		let ir = Range(utf16Range: r, in: self)
		guard let newtop = top.settingAttributes(attrs, range: ir)
		    else {
			return
		}
		top = newtop
	}
	public func clearAttributesOnRange(_ range: Range<Offset>) {
		top = top.clearingAttributes(range: range)
	}
}

extension Rope {
	func indices(follow target: Handle) -> Bool? {
		return top.indices(follow: target)
	}
	func indices(precede target: Handle) -> Bool? {
		return top.indices(precede: target)
	}
}

extension Rope {
	public convenience init(with node: Node) {
		self.init()
		top = node
	}
}

extension Rope {
        public struct UTF16View {
		let rope: Rope
		init(rope r: Rope) {
			rope = r
		}
                public subscript(i: Offset) -> Unicode.UTF16.CodeUnit {
                        get {
                                return rope.top.utf16(at: i)
                        }
                }
		public var length: Int {
			return rope.top.length
		}
	}
	public var utf16: UTF16View {
                return UTF16View(rope: self)
	}
}

extension Rope {
	func extents(enclosing i: Index) -> [ExtentController]? {
		return top.extents(enclosing: i)
	}
	public func extentsClosing(at i: Index)
	    -> [ExtentController]? {
		return top.extentsClosing(at: i)
	}
	public func extentsOpening(at i: Index)
	    -> [ExtentController]? {
		return top.extentsOpening(at: i)
	}
}

extension Rope {
	public func index(after i: Index, climbing dir: Climb,
	    bottom: inout ExtentController?) -> Index? {
		if case .end(_) = i {
			return nil
		}
		let j = index(after: i)
		switch (extents(enclosing: i)?.count,
			dir,
		        extents(enclosing: j)?.count) {
		case (let ni?, .in, let nj?) where ni < nj:
			return j
		case (let ni?, .out, let nj?) where ni > nj:
			return j
		default:
			return nil
		}
	}
	public func index(before i: Index, climbing dir: Climb,
	    bottom: inout ExtentController?) -> Index? {
		if case .start(_) = i {
			return nil
		}
		let j = index(before: i)
		switch (extents(enclosing: i),
			dir,
		        extents(enclosing: j)) {
		case (let ei?, .in, let ej?) where ei.count < ej.count:
			bottom = ej.last
			return j
		case (let ei?, .out, let ej?) where ei.count > ej.count:
			bottom = ej.last
			return j
		default:
			return nil
		}
	}
	public func index(after i: Index, climbing dir: Climb) -> Index? {
		var discard: ExtentController?
		return index(after: i, climbing: dir, bottom: &discard)
	}
	public func index(before i: Index, climbing dir: Climb) -> Index? {
		var discard: ExtentController?
		return index(before: i, climbing: dir, bottom: &discard)
	}
}

public func commonPrefix<S>(_ s1: S, _ s2: S)
    -> [S.Element] where S : Sequence, S.Element : Equatable {
	return zip(s1, s2).prefix { (e1, e2) in e1 == e2 }.map { (e, _) in e }
}

extension Rope {
	public func tightened(selection: Range<Index>)
	    -> (range: Range<Index>,
	        leftControllers: [ExtentController],
	        rightControllers: [ExtentController])? {
		var (l, r) = (selection.lowerBound, selection.upperBound)
		var lo, ro: [ExtentController]
		/* TBD move extents(enclosing:) calls out of loop, use
		 * index(after/before: ..., climbing: .in, bottom: ...) to
		 * get the next deeper extent at each step
		 */
		while true {
			switch (extents(enclosing: l), extents(enclosing: r)) {
			case (let _lo?, let _ro?):
				lo = _lo
				ro = _ro
			default:
				return nil
			}
			if l == r {
				assert(lo == ro)
				return (l..<r, lo, lo)
			}
			if lo.count < ro.count,
			   let next = index(after: l, climbing: .in) {
				l = next
			} else if lo.count > ro.count,
			          let next = index(before: r, climbing: .in) {
				r = next
			} else if lo.count == ro.count,
			          let lnext = index(after: l, climbing: .in),
			    let rnext = index(before: r, climbing: .in) {
				l = lnext
				r = rnext
			} else {
				return (l..<r, lo, ro)
			}
		}
	}
	public func rightLoosened(selection s: Range<Index>,
	    limit: ExtentController?) -> Range<Index>? {
		let l = s.lowerBound
		var r = s.upperBound
		var bottom: ExtentController?
		guard let extents = extents(enclosing: r) else {
			return nil
		}
		if extents.last == limit {
			return l..<r
		}
		while let next = index(after: r, climbing: .out,
		    bottom: &bottom) {
			r = next
			if bottom == limit {
				break
			}
		}
		return l..<r
	}
	public func leftLoosened(selection s: Range<Index>,
	    limit: ExtentController?) -> Range<Index>? {
		var l = s.lowerBound
		let r = s.upperBound
		var bottom: ExtentController?
		guard let extents = extents(enclosing: l) else {
			return nil
		}
		if extents.last == limit {
			return l..<r
		}
		while let next = index(before: l, climbing: .out,
		    bottom: &bottom) {
			l = next
			if bottom == limit {
				break
			}
		}
		return l..<r
	}
	public func directed(selection s: Range<Index>)
	    -> (range: Range<Index>,
	        narrow: ExtentController?,
	        wide: ExtentController?)? {
		guard let (tight, lo, ro) = tightened(selection: s) else {
			return nil
		}
		/* Loosen `tight` inside the innermost controller and
		 * return the loosened range.
		 */
		let common = commonPrefix(lo, ro)
		let looser = leftLoosened(selection: tight,
		    limit: common.last)!
		let loosest = rightLoosened(selection: looser,
		    limit: common.last)!
		return (range: loosest, narrow: common.last, wide: common.first)
	}
}

extension Rope {
	public func index(_ i: Index, offsetBy _times: Int) -> Index {
		var times = _times
		var result: Index = i
		while times < 0 {
			result = index(before: result)
			times = times + 1
		}
		while times > 0 {
			result = index(after: result)
			times = times - 1
		}
		return result
	}
}

/*
extension Rope : ExpressibleByStringLiteral,
    ExpressibleByExtendedGraphemeClusterLiteral where
    Rope.Content : ExpressibleByStringLiteral {
	public init(stringLiteral s: S) {
		top = Rope.Node(content: s)
	}
}
*/
