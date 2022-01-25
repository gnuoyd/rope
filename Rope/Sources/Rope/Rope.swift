//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
import Foundation

enum RopeNoSuchElement : Error {
case onInterior
}

struct BoundarySet : OptionSet {
	let rawValue: Int
	static let left = BoundarySet(rawValue: 1 << 0)
	static let right = BoundarySet(rawValue: 1 << 1)
	static let both: BoundarySet = [.left, .right]
	static let neither: BoundarySet = []
}

extension Range {
	public init<C : Content>(_ r: NSRange,
	    within view: Rope<C>.RopeAxisView) where Bound == Rope<C>.Index {
		self.init(r.range, within: view)
	}
	public init<C : Content>(_ r: Range<Int>,
	    within view: Rope<C>.RopeAxisView) where Bound == Rope<C>.Index {
		let lower = Rope<C>.Index(abutting: r.lowerBound, on: .right,
		    within: view)
		let upper = Rope<C>.Index(abutting: r.upperBound, on: .left,
		    within: view)
		if r.lowerBound != r.upperBound {
			self = lower..<upper
		} else if try! view.rope.node.label(lower.label,
		                                     precedes: upper.label,
						     by: .jot) {
			self = lower..<upper
		} else {
			self = upper..<lower
		}
	}
	public func relative<C : Content>(to rope: Rope<C>,
	    on dimension: KeyPath<Rope<C>.Node.Dimensions, Int>)
	    throws -> Range<Int> where Bound == Rope<C>.Index {
		let lower = try rope.offset(of: lowerBound, on: dimension)
		let upper = try rope.offset(of: upperBound, on: dimension)
		return lower..<upper
	}
	public func relative<C : Content>(to view: Rope<C>.RopeAxisView)
	    throws -> Range<Int> where Bound == Rope<C>.Index {
		return try relative(to: view.rope, on: view.axis)
	}
}

/* Use cases:
 *
 * Get/set/add/remove attributes on characters.
 *
 * Enclose a range in a zone.  The range must be
 * well-formed: must begin and end inside the same zone.
 *
 * "Step" an index left or right by a character.
 *
 * "Scoot" an index left or right by a zone boundary.
 *
 * "Scoot" an index left or right by a label.
 */
public class Rope<C : Content> {
	public enum Climb {
	case `in`
	case out
	}
	public typealias Content = C
	public typealias Element = Node
	public typealias Offset = Node.Offset
	public enum Index : Comparable {
	case interior(of: Rope, label: Label)
	}
	public class ZoneController : Label {
		public override var id: Id { return .zone(_id) }
		func subrope(of content: Rope.Node, after boundary: Rope.Index,
		    properties props: ZoneProperties,
		    depth: Int = 0) -> Rope.Node? {
			guard let subcontent = content.subrope(after: boundary,
			    depth: depth) else {
				return nil
			}
			return .zone((self, props), subcontent)
		}
		func subrope(of content: Rope.Node, upTo boundary: Rope.Index,
		    properties props: ZoneProperties,
		    depth: Int = 0) -> Rope.Node? {
			guard let subcontent = content.subrope(upTo: boundary,
			    depth: depth) else {
				return nil
			}
			return .zone((self, props), subcontent)
		}
		func setController(
		    _ z: (ctlr: ZoneController, props: ZoneProperties),
		    after lowerBound: Label, upTo upperBound: Label,
		    in content: Rope.Node, properties props: ZoneProperties,
		    undoList: ChangeList<Rope.Node>?) throws -> Rope.Node {
			return .zone((self, props),
			    try content.setController(z,
			        after: lowerBound, upTo: upperBound,
				undoList: undoList))
		}
		func replacing(after lowerBound: Label, upTo upperBound: Label,
		    in content: Rope.Node, properties props: ZoneProperties,
		    with replacement: Rope.Node,
		    undoList: ChangeList<Rope.Node>?) throws -> Rope.Node {
			return .zone((self, props), try content.replacing(
			    after: lowerBound, upTo: upperBound,
			    with: replacement, undoList: undoList))
		}
		func transformingAttributes(after lowerBound: Label,
		    upTo upperBound: Label, in content: Rope.Node,
		    properties props: ZoneProperties,
		    andBoundaries boundaries: BoundarySet = .neither,
		    with fn: (Attributes) -> Attributes) throws -> Rope.Node {
			let xformed = try content.transformingAttributes(
			    after: lowerBound, upTo: upperBound, with: fn)
			return .zone((self, props), xformed)
		}
	}
	public struct BoundaryUnits {
		let open: Content.Unit
		let close: Content.Unit
		public init(open o: Content.Unit, close c: Content.Unit) {
			self.open = o
			self.close = c
		}
	}
	public struct BoundaryAttributes {
		let open: Attributes
		let close: Attributes
		public init(open o: Attributes, close c: Attributes) {
			self.open = o
			self.close = c
		}
	}
	public struct BoundaryProperties {
		let attributes: BoundaryAttributes
		let units: BoundaryUnits
		public init(attributes a: BoundaryAttributes,
		            units u: BoundaryUnits) {
			self.attributes = a
			self.units = u
		}
	}

	public struct ZoneProperties : Equatable {
		public static func ==(_ l: Self, _ r: Self) -> Bool {
			return true
		}
		public init() {
		}
	}
	/* Any number of indices can appear between two character positions,
	 * between zone boundaries, at the start and at the end of a Rope.
	 */
	public indirect enum Node {
	public typealias Content = C
	public typealias Element = C.Element
	public typealias Offset = Int
	case index(Weak<Label>)
	case zone((ZoneController, ZoneProperties), Node)
	case concat(Node, Dimensions, UInt, LabelSet, Node, Dimensions)
	case leaf(Attributes, C)
	case empty
	}
	typealias OffsetPair = (lower: Offset, upper: Offset)

	public struct AxisDelegates {
		let rope: Rope<C>
		var delegate:
		    [KeyPath<Rope<C>, RopeAxisView> : AnyRopeDelegate<C>] = [:]
		public subscript(_ path: KeyPath<Rope<C>, RopeAxisView>)
		    -> RopeDelegate {
			get {
				return delegate[path] ??
				    AnyRopeDelegate(
				        didChange: rope.ropeDidChange,
					attributesDidChange:
					    rope.ropeAttributesDidChange)
			}
			set (d) {
				delegate[path] = AnyRopeDelegate(
				    didChange: d.ropeDidChange,
				    attributesDidChange:
				       d.ropeAttributesDidChange)
			}
		}
		func axisRanges(for range: Range<Rope.Index>) throws
		    -> [KeyPath<Rope, RopeAxisView> : Range<Int>] {
			var axisRange:
			    [KeyPath<Rope, RopeAxisView> : Range<Int>] = [:]
			for axis in delegate.keys {
				axisRange[axis] =
				    try range.relative(to: rope[keyPath: axis])
			}
			return axisRange
		}
		func indicateChanges<T>(
		    new: [KeyPath<Rope, RopeAxisView> : Range<Int>],
		    old: [KeyPath<Rope, RopeAxisView> : Range<Int>],
		    undoList: ChangeList<Rope<T>>) {
			for (axis, rNew) in new {
				guard let rOld = old[axis],
				      let d = delegate[axis] else {
					continue
				}
				d.indicateChanges(new: rNew, old: rOld,
					    undoList: undoList)
			}
		}
		func indicateAttributeChanges<T>(
		    on affected: [KeyPath<Rope, RopeAxisView> : Range<Int>],
		    undoList: ChangeList<Rope<T>>? = nil) {
			for (axis, range) in affected {
				guard let d = delegate[axis] else {
					continue
				}
				d.indicateAttributeChanges(on: range,
				    undoList: undoList)
			}
		}
		init(of r: Rope<C>) {
			rope = r
		}
	}
	private var _axisDelegates: AxisDelegates? = nil
	public var axisDelegates: AxisDelegates {
		get {
			guard let d = _axisDelegates else {
				let d = AxisDelegates(of: self)
				_axisDelegates = d
				return d
			}
			return d
		}
		set (d) {
			_axisDelegates = d
		}
	}
	var _boundaryProperties: BoundaryProperties? = nil
	public var boundaryProperties: BoundaryProperties {
		get {
			return _boundaryProperties ?? BoundaryProperties(
			    attributes: BoundaryAttributes(open: [:],
			                                   close: [:]),
			    units: BoundaryUnits(open: Content.Unit.default,
			                         close: Content.Unit.default))
		}
		set {
			_boundaryProperties = newValue
		}
	}
	private var mutations: UInt = 0
	private var rebalanceInterval: UInt = 32
	internal var _startLabel: Label
	internal var _endLabel: Label
	private var _top: Node
	private var top: Node {
		get {
			return _top
		}
		set {
			var tmp: Node
			mutations += 1
			if mutations.isMultiple(of: rebalanceInterval) {
				tmp = newValue.cleaned() ?? .empty
			} else {
				tmp = newValue
			}
			switch try? tmp.any(.jot, precedes: _startLabel) {
			case false?:
				break
			default:
				_startLabel = Label()
				tmp = .nodes(.index(label: _startLabel), tmp)
			}
			switch try? tmp.any(.jot, follows: _endLabel) {
			case false?:
				break
			default:
				_endLabel = Label()
				tmp = .nodes(tmp, .index(label: _endLabel))
			}
			if mutations.isMultiple(of: rebalanceInterval) {
				_top = tmp.rebalanced()
			} else {
				_top = tmp
			}
		}
	}
	public var startIndex: Index {
		return .interior(of: self, label: _startLabel)
	}
	public var endIndex: Index {
		return .interior(of: self, label: _endLabel)
	}
	public init() {
		_startLabel = Label()
		_endLabel = Label()
		_top = .nodes(.index(label: _startLabel),
		              .index(label: _endLabel))
	}
	public init(with nodes: Node...) {
		let prototype = Node.tree(from: nodes)
		var tmp = prototype
		if let leftmost = prototype.leftmostIndexLabel(),
		    case false? = try? prototype.any(.jot, precedes: leftmost) {
			_startLabel = leftmost
		} else {
			_startLabel = Label()
			tmp = .nodes(.index(label: _startLabel), tmp)
		}
		if let rightmost = prototype.rightmostIndexLabel(),
		    case false? = try? prototype.any(.jot, follows: rightmost) {
			_endLabel = rightmost
		} else {
			_endLabel = Label()
			tmp = .nodes(tmp, .index(label: _endLabel))
		}
		_top = tmp
	}
	public var node: Node {
		get {
			return top
		}
		set {
			top = newValue
		}
	}
	public func indicatingChangesModifyTop(_ f: (Node) -> Node) {
		let oldOffsets =
		    try! axisDelegates.axisRanges(for: startIndex..<endIndex)
		top = f(top)
		let newOffsets =
		    try! axisDelegates.axisRanges(for: startIndex..<endIndex)
		let throwaway = ChangeList<Rope>()
		axisDelegates.indicateChanges(new: newOffsets,
		    old: oldOffsets, undoList: throwaway)
	}
	public func label(_ h1: Label, precedes h2: Label,
	    by ival: Rope.Node.Interval) throws -> Bool {
		return try top.label(h1, precedes: h2, by: ival)
	}
	public func label(_ h1: Label, aliases h2: Label) throws -> Bool {
		if h1 == h2 {
			return true
		}
		let precedes = try top.label(h1, precedes: h2, by: .step)
		let follows = try top.label(h2, precedes: h1, by: .step)
		return !precedes && !follows
	}
	public init<T>(content t: T) where C.SubSequence == T {
		_startLabel = Label()
		_endLabel = Label()
		_top = .nodes(.index(label: _startLabel),
		              Node(content: t),
			      .index(label: _endLabel))
	}
	public func index(after i: Index) -> Index {
		guard i.owner === self else {
			fatalError("Mismatched owner")
		}
		let j = Label()
		switch top.inserting(j, one: .rightStep, after: i.label) {
		case .inchOut:
			fatalError(".interior(_, \(i.label)) already at end?")
		case .absent:
			fatalError(".interior(_, \(i.label)) is absent")
		case .stepOut:
			return endIndex
		case .step(let node):
			top = node
			return .interior(of: self, label: j)
		}
	}
	public func index(before i: Index) -> Index {
		guard i.owner === self else {
			fatalError("Mismatched owner")
		}
		let j = Label()
		switch top.inserting(j, one: .leftStep, after: i.label) {
		case .inchOut:
			fatalError(
			    ".interior(_, \(i.label)) already at start?")
		case .absent:
			fatalError(".interior(_, \(i.label)) is absent")
		case .stepOut:
			return startIndex
		case .step(let node):
			top = node
			return .interior(of: self, label: j)
		}
	}
	public func replace(_ r: Range<Index>, with replacement: Content,
	    undoList: ChangeList<Rope>) throws {
		/* Create a ChangeList and record .replacing(...) changes on
		 * it. .replacing(...) has labels each change location.
		 *
		 * Apply the ChangeList to `top` to produce `newtop`,
		 * recording the inverse on undoList.
		 *
		 * Finally, record an indication that a text range
		 * was edited with its offsets and any change in length.
		 */
		let changes = ChangeList<Rope.Node>()
		let oldOffsets = try axisDelegates.axisRanges(for: r)
		top = try top.replacing(
		    after: r.lowerBound, upTo: r.upperBound,
		    with: replacement, undoList: changes)
		let newOffsets = try axisDelegates.axisRanges(for: r)
		undoList.record { (rope, undoList) in
			try rope.applyChanges(changes, undoList: undoList)
			return rope
		}
		axisDelegates.indicateChanges(new: newOffsets, old: oldOffsets,
		    undoList: undoList)
	}
	public func setController(_ ctlr: ZoneController,
	    on r: Range<Index>, undoList: ChangeList<Rope>) throws {
		let changes = ChangeList<Rope.Node>()
		let oldOffsets = try axisDelegates.axisRanges(for: r)
		top = try top.setController((ctlr, ZoneProperties()),
		    after: r.lowerBound.label, upTo: r.upperBound.label,
		    undoList: changes)
		let newOffsets = try axisDelegates.axisRanges(for: r)
		undoList.record { (rope, undoList) in
			try rope.applyChanges(changes, undoList: undoList)
			return rope
		}
		axisDelegates.indicateChanges(new: oldOffsets, old: newOffsets,
		    undoList: undoList)
	}
	func applyChanges(_ changes: ChangeList<Rope.Node>,
	     undoList: ChangeList<Rope>) throws {
		let (newtop, reversals) = try changes.play(withTarget: top)
		top = newtop
		undoList.record { (rope, undoList) in
			try self.applyChanges(reversals, undoList: undoList)
			return rope
		}
	}
	public func setAttributes(_ attrs: Attributes, range r: Range<Index>) {
		guard let newtop = try? top.settingAttributes(attrs, range: r)
		    else {
			return
		}
		top = newtop
		guard let axisRanges = try? axisDelegates.axisRanges(for: r)
		    else {
			return
		}
		axisDelegates.indicateAttributeChanges(on: axisRanges,
		    undoList: nil as ChangeList<Rope>?)
	}
	public func clearAttributes(on r: Range<Index>) {
		guard let newtop = try? top.clearingAttributes(on: r) else {
			return
		}
		top = newtop
		guard let axisRanges = try? axisDelegates.axisRanges(for: r)
		    else {
			return
		}
		axisDelegates.indicateAttributeChanges(on: axisRanges,
		    undoList: nil as ChangeList<Rope>?)
	}
	public func offset(of index: Index,
	    on dimension: KeyPath<Rope.Node.Dimensions, Int>) throws -> Int {
		guard case .interior(_, let label) = index else {
			throw RopeNoSuchElement.onInterior
		}
		return try top.offset(of: label, on: dimension)
	}
}

extension Rope : RopeDelegate {
	public func ropeDidChange(on: Range<Int>, changeInLength: Int) {
		return
	}
	public func ropeAttributesDidChange(on: Range<Int>) {
		return
	}
}

extension Rope {
	var hasSingleIndex: Bool {
		return top.hasSingleIndex
	}
}

extension Rope.Node {
	var hasSingleIndex: Bool {
		return dimensions.steps == 0
	}
}

extension Rope {
	func zonesEnclosing(_ i: Index) throws -> [ZoneController] {
		return try top.zonesEnclosing(i)
	}
	public func zonesClosing(at i: Index) throws -> [ZoneController] {
		return try top.zonesClosing(at: i)
	}
	public func zonesOpening(at i: Index) throws -> [ZoneController] {
		return try top.zonesOpening(at: i)
	}
}

extension Rope {
	public func index(after i: Index, climbing dir: Climb,
	    innermostZone: inout ZoneController?) -> Index? {
		do {
			if try !top.any(.jot, follows: i.label) {
				return nil
			}
		} catch {
			return nil
		}
		let j = index(after: i)
		switch (try? zonesEnclosing(i), dir, try? zonesEnclosing(j)) {
		/* ((.(( -> (((.(
		 *   i   ->    j
		 */
		case (let zi?, .in, let zj?) where zi.count < zj.count:
			innermostZone = zj.last
			return j
		/* )).)) -> ))).)
		 *   i   ->    j
		 */
		case (let zi?, .out, let zj?) where zi.count > zj.count:
			innermostZone = zj.last
			return j
		default:
			return nil
		}
	}
	public func index(before j: Index, climbing dir: Climb,
	    innermostZone: inout ZoneController?) -> Index? {
		do {
			if try !top.any(.jot, precedes: j.label) {
				return nil
			}
		} catch {
			return nil
		}
		let i = index(before: j)
		switch (try? zonesEnclosing(i), dir, try? zonesEnclosing(j)) {
		/* )).)) <- ))).)
		 *   i   <-    j
		 */
		case (let zi?, .in, let zj?) where zi.count > zj.count:
			innermostZone = zi.last
			return i
		/* ((.(( <- (((.(
		 *   i   <-    j
		 */
		case (let zi?, .out, let zj?) where zi.count < zj.count:
			innermostZone = zi.last
			return i
		default:
			return nil
		}
	}
	public func index(after i: Index, climbing dir: Climb) -> Index? {
		var discard: ZoneController?
		return index(after: i, climbing: dir, innermostZone: &discard)
	}
	public func index(before i: Index, climbing dir: Climb) -> Index? {
		var discard: ZoneController?
		return index(before: i, climbing: dir, innermostZone: &discard)
	}
}

extension Rope {
	public func tightenedSelection(_ selection: Range<Index>)
	    throws -> (range: Range<Index>,
	               leftControllers: [ZoneController],
	               rightControllers: [ZoneController]) {
		var (l, r) = (selection.lowerBound, selection.upperBound)
		/* TBD move zonesEnclosing() calls out of loop, use
		 * index(after/before: ..., climbing: .in, innermostZone: ...)
		 * to get the next deeper zone at each step
		 */
		while true {
			let (lo, ro) = (try zonesEnclosing(l),
			                try zonesEnclosing(r))
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
	    limit: ZoneController?) throws -> Range<Index> {
		let l = s.lowerBound
		var r = s.upperBound
		var innermostZone: ZoneController?
		let zones = try zonesEnclosing(r)
		if zones.last == limit {
			return l..<r
		}
		while let next = index(after: r, climbing: .out,
		    innermostZone: &innermostZone) {
			r = next
			if innermostZone == limit {
				break
			}
		}
		return l..<r
	}
	public func leftLoosenedSelection(_ s: Range<Index>,
	    limit: ZoneController?) throws -> Range<Index> {
		var l = s.lowerBound
		let r = s.upperBound
		var innermostZone: ZoneController?
		let zones = try zonesEnclosing(l)
		if zones.last == limit {
			return l..<r
		}
		while let next = index(before: l, climbing: .out,
		    innermostZone: &innermostZone) {
			l = next
			if innermostZone == limit {
				break
			}
		}
		return l..<r
	}
	public func directedSelection(_ s: Range<Index>)
	    throws -> (range: Range<Index>,
	               narrow: ZoneController?,
	               wide: ZoneController?) {
		let (tight, lo, ro) = try tightenedSelection(s)
		/* Loosen `tight` inside the innermost controller and
		 * return the loosened range.
		 */
		let common = commonPrefix(lo, ro)
		let looser =
		    try leftLoosenedSelection(tight, limit: common.last)
		let loosest =
		    try rightLoosenedSelection(looser, limit: common.last)
		return (range: loosest, narrow: common.last, wide: common.first)
	}
}

extension Rope {
	public func firstIndex(inZone target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingFirstIndex(label,
		    inZone: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func lastIndex(inZone target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingLastIndex(label,
		    inZone: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func index(afterZone target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingIndex(label,
		    afterZone: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func index(beforeZone target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingIndex(label,
		    beforeZone: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
}

/*
extension Rope : ExpressibleByStringLiteral where
    Rope.Content : ExpressibleByStringLiteral {
	public convenience init<S>(stringLiteral s: S) where S : ExpressibleByStringLiteral {
		self = Rope(content: Rope.Content(s))
	}
}
*/

extension RopeDelegate {
	func indicateAttributeChanges<T>(on range: Range<Int>,
	    undoList: ChangeList<Rope<T>>? = nil) {
		undoList?.record { (rope, undoList) in
			self.indicateAttributeChanges(
			    on: range, undoList: undoList)
			return rope
		}
		ropeAttributesDidChange(on: range)
	}
	func indicateChanges<T>(new: Range<Int>, old: Range<Int>,
	    undoList: ChangeList<Rope<T>>) {
		undoList.record { (rope, undoList) in
			self.indicateChanges(
			    new: old,
			    old: new,
			    undoList: undoList)
			return rope
		}
		ropeDidChange(on: old,
		    changeInLength: new.count - old.count)
	}
}
