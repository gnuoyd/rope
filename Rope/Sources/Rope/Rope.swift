//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
import Foundation

enum RopeNoSuchElement : Error {
case onInterior
case atStart
case atEnd
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
	public typealias Content = C
	public typealias Element = Node<C>
	public typealias Index = RopeIndex<C>
	public var top: Node<C>
	public var generation: UInt64 = 0
	public var startIndex: Index {
		if top.startIndex == top.endIndex {
			return .end(of: self)
		}
		return .start(of: self)
	}
	public var endIndex: Index { return .end(of: self) }

	public init() {
		top = .empty
	}
	public var node: Node<C> {
		get {
			return top
		}
		set {
			top = newValue
		}
	}
	public func containsIndex(_ h1: Handle, before h2: Handle) -> Bool {
		return top.containsIndex(h1, before: h2)
	}
	public init<T>(content t: T) where C : Initializable,
	    C.Initializer == T, T : Collection {
		top = Node(content: t)
		Swift.print("Rope initialized with content \(top), utf16 length \(utf16.length)")
	}
	public func index(after i: Index) -> Index {
		guard i.owner === self else {
			fatalError("Mismatched owner")
		}
		switch i {
		case .start(_):
			let h = Handle()
			guard case
			    .step(let n) = top.afterStepInsertingIndex(h) else {
				return .end(of: self)
			}
			top = n
			return .interior(of: self, at: generation,
			                 index: 0, handle: h)
		case .end(_):
			fatalError("No index after .endIndex")
		case .interior(_, _, let m, let h):
			let j = Handle()
			switch top.insertingIndex(j, oneStepAfter: h) {
			case .inchOut:
				fatalError(
				    ".interior(\(m), \(h)) already at end?")
			case .absent:
				fatalError(
				    ".interior(\(m), \(h)) is absent")
			case .stepOut:
				return .end(of: self)
			case .step(let node):
				top = node
				return .interior(of: self,
				                 at: generation,
						 index: m + 1,
						 handle: j)
			}
		}
	}
	/* TBD tests */
	public subscript(_ r: Range<NodeIndex>) -> Content {
		set(newValue) {
			top = top.replacing(range: r, with: newValue)
		}
		get {
			return top[r]
		}
	}
/*
	public subscript<I>(_ r: Range<NodeIndex>) -> I
                where C : Initializable, C.Initializer == I, I : Collection, I : Initializable, I.Initializer == C {
		set(newValue) {
			top = top.replacing(range: r, with: C(newValue))
		}
		get {
			return I(top[r])
		}
	}
*/
	public subscript(i: NodeIndex) -> Content.Element {
		return top.element(at: i)
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
			guard case .step(let node) = top.firstElement()
			    else {
				throw RopeNoSuchElement.atStart
			}
			return node
		case .interior(_, _, _, let h):
			let result = top.element(at: h)
			guard case .step(let node) = result else {
				throw RopeNoSuchElement.onInterior
			}
			return node
		case .end(_):
			throw RopeNoSuchElement.atEnd
		}
	}
	public func insert(_ elt: Element, at i: Index) {
		guard self === i.owner else {
			fatalError("Invalid index")
		}
		if case .empty = elt {
			fatalError("You may not insert .empty")
		}
		switch i {
		case .start(_):
			top = Node<C>(left: elt, right: top)
		case .end(_):
			top = Node(left: top, right: elt)
		case .interior(_, _, _, let h):
			top = top.inserting(elt, at: h)
		}
	}
	public func attributes(at i: NodeIndex)
	    -> (Attributes, Range<NodeIndex>) {
		return top.attributes(at: i)
	}
	public func setAttributes(_ attrs: Attributes, range: Range<NodeIndex>){
		top = top.settingAttributes(attrs, range: range)
	}
	public func clearAttributesOnRange(_ range: Range<NodeIndex>) {
		top = top.clearingAttributes(range: range)
	}
}

extension Rope {
        public struct UTF16View {
		let rope: Rope<C>
		init(rope r: Rope<C>) {
			rope = r
		}
                public subscript(i: NodeIndex) -> Unicode.UTF16.CodeUnit {
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

/*
extension Rope : ExpressibleByStringLiteral,
    ExpressibleByExtendedGraphemeClusterLiteral where
    Rope.Content : ExpressibleByStringLiteral {
	public init(stringLiteral s: S) {
		top = Node<Content>(content: s)
	}
}
*/
