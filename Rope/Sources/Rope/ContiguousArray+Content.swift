extension ContiguousArray : UnitViewable where Element == UTF16.CodeUnit {
    public typealias Unit = Element
    
    public typealias UnitView = Self
    
}

extension ContiguousArray : Content where Element == UTF16.CodeUnit {
}
