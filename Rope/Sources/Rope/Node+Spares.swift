public extension Rope.Node {
	func extractContent(from start: Offset, upTo end: Offset)
	    -> C.SubSequence {
		switch self {
		case .concat(let l, let mid, _, _, let r, _):
			var c = C.empty
			if start < mid.units {
				c += l.extractContent(from: start,
				    upTo: min(end, mid.units))
			}
			if mid.units < end {
				c += r.extractContent(
				    from: max(start, mid.units) - mid.units,
				    upTo: end - mid.units)
			}
			return c[...]
		case .leaf(_, let s):
			guard let sidx = s.units.index(s.units.startIndex,
			    offsetBy: start, limitedBy: s.units.endIndex),
			    let eidx = s.units.index(s.units.startIndex,
			    offsetBy: end, limitedBy: s.units.endIndex) else {
				fatalError("In \(#function), " +
				    "no units range \(start)..<\(end)")
			}
			return s[sidx..<eidx]
		case .zone(_, let content):
			return content.extractContent(from: start, upTo: end)
		case .empty, .index(_):
			return C.empty[...]
		}
	}
	func extractContent(from start: Offset, upTo end: Offset,
	    filling c: inout C) {
		switch self {
		case .concat(let l, let mid, _, _, let r, _):
			if start < mid.units {
				l.extractContent(from: start,
				    upTo: min(end, mid.units), filling: &c)
			}
			if mid.units < end {
				r.extractContent(
				    from: max(start, mid.units) - mid.units,
				    upTo: end - mid.units, filling: &c)
			}
		case .leaf(_, let s):
			guard let sidx = s.units.index(s.units.startIndex,
			    offsetBy: start, limitedBy: s.units.endIndex),
			    let eidx = s.units.index(s.units.startIndex,
			    offsetBy: end, limitedBy: s.units.endIndex) else {
				fatalError("In \(#function), " +
				    "no units range \(start)..<\(end)")
			}
			c += s[sidx..<eidx]
		case .zone(_, let content):
			content.extractContent(from: start, upTo: end,
			    filling: &c)
		case .empty, .index(_):
			return
		}
	}
}
