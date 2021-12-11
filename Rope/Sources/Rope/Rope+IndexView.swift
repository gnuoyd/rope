extension Rope {
        public struct IndexView : BidirectionalCollection {
		let rope: Rope
		public typealias Index = Rope.Index
		public var startIndex: Index {
			return rope.startIndex
		}
		public var endIndex: Index {
			return rope.endIndex
		}
		init(rope r: Rope) {
			rope = r
		}
		public subscript(i: Index) -> Index {
			return i
		}
		public func index(after i: Index) -> Index {
			return rope.index(after: i)
		}
		public func index(before i: Index) -> Index {
			return rope.index(before: i)
		}
	}
	public var indices: IndexView {
                return IndexView(rope: self)
	}
}
