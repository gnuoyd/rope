//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
extension Rope {
        public struct NestedView : BidirectionalCollection {
		let view: IndexView
		public typealias Index = Rope.Index
		public var startIndex: Index {
			return view.startIndex
		}
		public var endIndex: Index {
			return view.endIndex
		}
		init(rope r: Rope) {
			view = IndexView(rope: r)
		}
		public subscript(i: Index) -> Element {
			get {
				return try! element(at: i)
			}
		}
		func element(at i: Index) throws -> Element {
			let result = view.rope.node.element(at: i.label)
			guard case .step(let node) = result else {
				throw RopeNoSuchElement.onInterior
			}
			return node
		}
		subscript(r: Range<Index>) -> Element {
			get {
				if r.isEmpty {
					return .empty
				}
				guard let e = view.rope.node.subrope(
				    after: r.lowerBound,
				    upTo: r.upperBound) else {
					fatalError("No such range")
				}
				return e
			}
		}
		public func index(after i: Index) -> Index {
			return view.index(after: i)
		}
		public func index(before i: Index) -> Index {
			return view.index(before: i)
		}
	}
	public var nests: NestedView {
                return NestedView(rope: self)
	}
}
