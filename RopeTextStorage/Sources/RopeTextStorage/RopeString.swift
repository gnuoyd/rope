//
// Copyright (c) 2020 David Young.  All rights reserved.
//
import AppKit
import Rope

class RopeString : NSString {
	typealias Offset = Rope<Substring>.Node.Offset
	typealias Content = Rope<Substring>.Node.Content
	let rope: Rope<Substring>
	init(rope r: Rope<Substring>) {
		rope = r
		super.init()
	}
	override init() {
		rope = Rope()
		super.init()
	}
	required init?(coder aDecoder: NSCoder) {
		fatalError()
	}
	required init?(pasteboardPropertyList propertyList: Any,
	    ofType type: NSPasteboard.PasteboardType) {
		fatalError()
	}
	public override var length: Int {
		return rope.utf16.length
	}
	override func character(at i: Int) -> unichar {
		let c = rope.utf16[Offset(of: i)]
		// Swift.print("character(at: \(i)) -> \(c)")
		return c
	}
	override func getCharacters(_ buffer_in: UnsafeMutablePointer<unichar>,
	    range: NSRange) {
		var buffer = buffer_in
		if false {
			rope.extractUTF16(Offset.utf16Range(range), filling: &buffer)
		} else if false {
			/* 1 try without intermediate `c`. */
                        for u in rope.extractContent(Offset.utf16Range(range)).utf16 {
                                buffer.initialize(to: u)
                                buffer += 1
                        }
		} else {
			/*
			 * 2 try passing inout String to extractContent.
			 */
			var c: Content = Content.empty
			rope.extractContent(Offset.utf16Range(range),
			    filling: &c)
			let result: Bool? = c.utf16.map { return $0 }.withContiguousStorageIfAvailable {
				guard let base = $0.baseAddress else {
					return false
				}
				buffer.initialize(from: base, count: range.length)
				return true
			}
			if !(result ?? false) {
				for u in c.utf16 {
					buffer.initialize(to: u)
					buffer += 1
				}
			}
		}
	}
}
