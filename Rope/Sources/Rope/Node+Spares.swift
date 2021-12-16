public extension Rope.Node {
	func extractContent(from start: Offset, upTo end: Offset)
	    -> C.SubSequence {
		switch self {
		case .concat(let l, let idx, _, _, let r, _):
			var c = C.empty
			if start < idx {
				c += l.extractContent(from: start,
				    upTo: min(end, idx))
			}
			if idx < end {
				c += r.extractContent(
				    from: max(start, idx) - idx,
				    upTo: end - idx)
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
		case .concat(let l, let idx, _, _, let r, _):
			if start < idx {
				l.extractContent(from: start,
				    upTo: min(end, idx), filling: &c)
			}
			if idx < end {
				r.extractContent(
				    from: max(start, idx) - idx,
				    upTo: end - idx, filling: &c)
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
