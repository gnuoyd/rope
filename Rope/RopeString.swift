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
		return 0
	}
	override func character(at i: Int) -> unichar {
		return 0 // rope[NodeIndex(utf16Offset: i)]
	}
}
