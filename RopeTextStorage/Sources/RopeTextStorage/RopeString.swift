//
// Copyright (c) 2020, 2021 David Young.  All rights reserved.
//
import AppKit
import Rope

class RopeString : NSString {
	public typealias Backing = Rope<ContiguousArray<UTF16.CodeUnit>>
	typealias Offset = Backing.Node.Offset
	typealias Content = Backing.Node.Content
	let backing: Backing
	init(with backing: Backing) {
		self.backing = backing
		super.init()
	}
	override init() {
		self.backing = Rope()
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
		return backing.units.length
	}
	override func character(at i: Int) -> unichar {
		let c = backing.units[i]
		// Swift.print("character(at: \(i)) -> \(c)")
		return c
	}
	override func getCharacters(_ buffer_in: UnsafeMutablePointer<unichar>,
	    range: NSRange) {
		var buffer = buffer_in
		backing.units.extract(range.range, filling: &buffer)
	}
	override func copy(with zone: NSZone? = nil) -> Any {
		return self
	}
}
