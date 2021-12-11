extension Rope {
        public struct UnitView {
		let rope: Rope
		init(rope r: Rope) {
			rope = r
		}
                public subscript(i: Offset) -> Rope.Node.Unit {
                        get {
                                return rope.node.unit(at: i)
                        }
                }
		public var length: Int {
			return rope.node.length
		}
		public subscript(_ r: Range<Offset>) -> Content {
			get {
				let ir = Range(r, in: rope)
				return rope.node[ir]
			}
		}
		public subscript<I>(_ r: Range<Offset>)
		    -> I where Content.SubSequence == I {
			get {
				return self[r][...]
			}
		}
	}
	public var units: UnitView {
                return UnitView(rope: self)
	}
}
