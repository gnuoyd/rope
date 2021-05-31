//
// Copyright (c) 2020 David Young.  All rights reserved.
//
import AppKit
import Rope

class RopeString : NSString {
	typealias Offset = Rope<ContiguousArray<UTF16.CodeUnit>>.Node.Offset
	typealias Content = Rope<ContiguousArray<UTF16.CodeUnit>>.Node.Content
	let rope: Rope<ContiguousArray<UTF16.CodeUnit>>
	init(rope r: Rope<ContiguousArray<UTF16.CodeUnit>>) {
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
		return rope.units.length
	}
	override func character(at i: Int) -> unichar {
		let c = rope.units[Offset(of: i)]
		// Swift.print("character(at: \(i)) -> \(c)")
		return c
	}
	override func getCharacters(_ buffer_in: UnsafeMutablePointer<unichar>,
	    range: NSRange) {
		var buffer = buffer_in
		rope.extractUnits(Offset.unitRange(range), filling: &buffer)
	}
	override func copy(with zone: NSZone? = nil) -> Any {
		return self
	}
}
