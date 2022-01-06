extension Rope {
        public struct UnitView : UnitRopeView {
		public typealias Cx = Rope.Content

		let rope: Rope

		init(rope r: Rope) {
			rope = r
		}
		public var length: Int {
			return rope.node.dimensions.units
		}
		public func attributes(at i: Int) -> (Attributes, Range<Int>) {
			return rope.node.attributes(atUnit: i)
		}
		public subscript(_ r: Range<Int>) -> Rope.Content {
			get {
				let ir = Range(r, within: self)
				return rope.node[ir]
			}
		}
		public func extract(_ range: Range<Int>,
		    filling buffer: inout UnsafeMutablePointer<Unit>) {
			return rope.node.extractUnits(from: range.lowerBound,
			    upTo: range.upperBound, filling: &buffer)
		}
	}
	public var units: UnitView {
                return UnitView(rope: self)
	}
}
