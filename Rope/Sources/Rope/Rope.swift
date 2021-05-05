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
	init<C : Content>(_ r: Range<Rope<C>.Node.Offset>,
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
		public let unitOffset: Offset
		public static var zero: Dimensions {
			return Dimensions(jots: 0, unitOffset: 0)
		}
		public init(jots: Int = 0, unitOffset offset: Offset = 0) {
			self.jots = jots
			self.unitOffset = offset
		}
		public var halfPerimeter: Int {
			guard case .offset(let n) = unitOffset else {
				return jots
			}
			return jots + n
		}
	}
}

extension Rope.Node.Dimensions {
	public static func +(_ l: Self, _ r: Self) -> Self {
		return Self(jots: l.jots + r.jots,
		            unitOffset: l.unitOffset + r.unitOffset)
	}
}

extension Rope.Node.Offset : ExpressibleByIntegerLiteral {
	public typealias IntegerLiteralType = Int
	public init(integerLiteral n: Self.IntegerLiteralType) {
		self = .offset(n)
	}
}

extension Rope.Node {
	public typealias Unit = C.Unit
}

extension Rope.Node.Offset {
	public init(of n: Int) {
		self = .offset(n)
	}
	public var unitOffset: Int {
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
	case interior(of: Rope, label: Label)
	}
	public class ExtentController : Label {
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
	case cursor(Label, Attributes)
	case index(Weak<Label>)
	case extent(ExtentController, Node)
	case concat(Node, Offset, UInt, LabelSet, Node, Dimensions)
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
		if top.startIndex == top.endIndex &&
		   top.labels.extentCount == 0 {
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
	public func index(_ h1: Label, precedes h2: Label) -> Bool? {
		return top.index(h1, precedes: h2)
	}
	public init<T>(content t: T) where C.SubSequence == T {
		_top = Node(content: t)
	}
	public func index(after i: Index) -> Index {
		guard i.owner === self else {
			fatalError("Mismatched owner")
		}
		switch i {
		case .start(_):
			let h = Label()
			guard case .step(let n) =
			    top.inserting(h, after: .rightStep) else {
				return .end(of: self)
			}
			top = n
			return .interior(of: self, label: h)
		case .end(_):
			fatalError("No index after .endIndex")
		case .interior(_, let h):
			let j = Label()
			switch top.inserting(j, one: .rightStep, after: h) {
			case .inchOut:
				fatalError(".interior(_, \(h)) already at end?")
			case .absent:
				fatalError(".interior(_, \(h)) is absent")
			case .stepOut:
				return .end(of: self)
			case .step(let node):
				top = node
				return .interior(of: self, label: j)
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
			let h = Label()
			guard case .step(let n) =
			    top.inserting(h, after: .leftStep) else {
				return .start(of: self)
			}
			top = n
			return .interior(of: self, label: h)
		case .interior(_, let h):
			let j = Label()
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
				return .interior(of: self, label: j)
			}
		}
	}
	public subscript(i: Offset) -> Content.Element {
		return top.element(at: i)
	}
	public subscript(r: Range<Index>) -> Element {
		get {
			guard let e = top.subrope(after: r.lowerBound,
			    upTo: r.upperBound) else {
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
/*
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
*/
	/* TBD tests */
	public subscript(_ r: Range<Offset>) -> Content {
		set(newValue) {
			let ir = Range(r, in: self)
			guard let newtop = top.replacing(ir,
			    with: newValue) else {
				fatalError("No such range")
			}
			top = newtop
		}
		get {
			let ir = Range(r, in: self)
			return top[ir]
		}
	}
	public subscript<I>(_ r: Range<Offset>) -> I where C.SubSequence == I {
		set(newValue) {
			self[r] = C(newValue)
		}
		get {
			return self[r][...]
		}
	}
	public func attributes(at i: Offset) -> (Attributes, Range<Offset>) {
		return top.attributes(at: i)
	}
	public func setAttributes(_ attrs: Attributes, range r: Range<Offset>){
		let ir = Range(r, in: self)
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
	func indices(follow target: Label) -> Bool? {
		return top.indices(follow: target)
	}
	func indices(precede target: Label) -> Bool? {
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
        public struct UnitView {
		let rope: Rope
		init(rope r: Rope) {
			rope = r
		}
                public subscript(i: Offset) -> Rope.Node.Unit {
                        get {
                                return rope.top.unit(at: i)
                        }
                }
		public var length: Int {
			return rope.top.length
		}
	}
	public var units: UnitView {
                return UnitView(rope: self)
	}
}

extension Rope {
	public func extractContent(_ range: Range<Offset>) -> C.SubSequence {
		return top.extractContent(from: range.lowerBound,
		    upTo: range.upperBound)
	}
	public func extractContent(_ range: Range<Offset>,
	    filling buffer: inout C) {
		top.extractContent(from: range.lowerBound,
		    upTo: range.upperBound, filling: &buffer)
	}
	public func extractUnits(_ range: Range<Offset>,
	    filling buffer: inout UnsafeMutablePointer<C.Unit>){
		return top.extractUnits(from: range.lowerBound,
		    upTo: range.upperBound, filling: &buffer)
	}
}

extension Rope {
	func extentsEnclosing(_ i: Index) -> [ExtentController]? {
		return top.extentsEnclosing(i)
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
		switch (extentsEnclosing(i)?.count,
			dir,
		        extentsEnclosing(j)?.count) {
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
		switch (extentsEnclosing(i),
			dir,
		        extentsEnclosing(j)) {
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
	public func tightenedSelection(_ selection: Range<Index>)
	    -> (range: Range<Index>,
	        leftControllers: [ExtentController],
	        rightControllers: [ExtentController])? {
		var (l, r) = (selection.lowerBound, selection.upperBound)
		var lo, ro: [ExtentController]
		/* TBD move extentsEnclosing() calls out of loop, use
		 * index(after/before: ..., climbing: .in, bottom: ...) to
		 * get the next deeper extent at each step
		 */
		while true {
			switch (extentsEnclosing(l), extentsEnclosing(r)) {
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
	public func rightLoosenedSelection(_ s: Range<Index>,
	    limit: ExtentController?) -> Range<Index>? {
		let l = s.lowerBound
		var r = s.upperBound
		var bottom: ExtentController?
		guard let extents = extentsEnclosing(r) else {
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
	public func leftLoosenedSelection(_ s: Range<Index>,
	    limit: ExtentController?) -> Range<Index>? {
		var l = s.lowerBound
		let r = s.upperBound
		var bottom: ExtentController?
		guard let extents = extentsEnclosing(l) else {
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
	public func directedSelection(_ s: Range<Index>)
	    -> (range: Range<Index>,
	        narrow: ExtentController?,
	        wide: ExtentController?)? {
		guard let (tight, lo, ro) = tightenedSelection(s) else {
			return nil
		}
		/* Loosen `tight` inside the innermost controller and
		 * return the loosened range.
		 */
		let common = commonPrefix(lo, ro)
		let looser = leftLoosenedSelection(tight, limit: common.last)!
		let loosest = rightLoosenedSelection(looser,
		    limit: common.last)!
		return (range: loosest, narrow: common.last, wide: common.first)
	}
}

extension Rope {
	public func firstIndex(inExtent target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingFirstIndex(label,
		    inExtent: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func lastIndex(inExtent target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingLastIndex(label,
		    inExtent: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func index(afterExtent target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingIndex(label,
		    afterExtent: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func index(beforeExtent target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingIndex(label,
		    beforeExtent: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
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
