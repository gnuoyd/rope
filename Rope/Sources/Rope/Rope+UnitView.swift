extension Rope {
        public struct UnitView {
		let rope: Rope
		init(rope r: Rope) {
			rope = r
		}
                public subscript(i: Int) -> Rope.Node.Unit {
                        get {
                                return rope.node.unit(at: i)
                        }
                }
		public var length: Int {
			return rope.node.length
		}
		public subscript(_ r: Range<Int>) -> Content {
			get {
				let ir = Range(r, within: rope.units)
				return rope.node[ir]
			}
		}
		public subscript<I>(_ r: Range<Int>)
		    -> I where Content.SubSequence == I {
			get {
				return self[r][...]
			}
		}
		public func attributes(at i: Int) -> (Attributes, Range<Int>) {
			return rope.node.attributes(at: i)
		}
		public func extract(_ range: Range<Int>,
		    filling buffer: inout UnsafeMutablePointer<C.Unit>) {
			return rope.node.extractUnits(from: range.lowerBound,
			    upTo: range.upperBound, filling: &buffer)
		}
	}
	public var units: UnitView {
                return UnitView(rope: self)
	}
}
