import Foundation

/*
 * Result of looking up an element of a Node
 */
public enum ElementResult<C : Content> {
case absent
case inchOut
case step(Node<C>)
}

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
 * Enclose a range in a container.  The range must be
 * well-formed: must begin and end inside the same container. 
 *
 * Insert a cursor between characters or nested between containers.
 *
 * Remove a cursor.
 *
 * "Step" a cursor left or right by a character.
 *
 * "Scoot" a cursor left or right by a container boundary.
 *
 * "Scoot" a cursor left or right by a cursor?
 *
 * Replace a cursor by a container; apply the cursor's attributes to
 * the container's content.
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

	public subscript(i: NodeIndex) -> Content.Element {
		return top.element(at: i)
	}
	public subscript(i: Index) -> Iterator.Element {
		get {
			do {
				return try element(at: i)
			} catch {
				fatalError("No such element")
			}
		}
        }
        public func element(at i: Index) throws -> Iterator.Element {
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
