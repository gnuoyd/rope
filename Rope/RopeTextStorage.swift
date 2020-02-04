class RopeTextStorage: NSTextStorage {
	let rope: Rope<Substring>
	override init() {
                rope = Rope()
                super.init()
        }
	required init?(coder: NSCoder) {
		fatalError("init(coder:) has not been implemented")
	}
        required init?(pasteboardPropertyList propertyList: Any, ofType type: NSPasteboard.PasteboardType) {
                fatalError("init(pasteboardPropertyList:ofType:) has not been implemented")
        }
        override func attributes(at location: Int,
	    effectiveRange range: NSRangePointer?)
	    -> [NSAttributedString.Key : Any] {
		return [:]
	}
	override func setAttributes(_ attrs: [NSAttributedString.Key : Any]?,
	    range: NSRange) {
		return
	}
}
