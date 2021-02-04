//
// Copyright (c) 2020 David Young.  All rights reserved.
//
import AppKit
import Rope

class RopeString : NSString {
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
		let c = rope.utf16[NodeIndex(utf16Offset: i)]
		// Swift.print("character(at: \(i)) -> \(c)")
		return c
	}
}
