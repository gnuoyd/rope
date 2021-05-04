extension Array : UnitViewable where Element == UTF16.CodeUnit {
    public typealias Unit = Element
    
    public typealias UnitView = Self
    
}

extension Array : Content where Element == UTF16.CodeUnit {
}

extension ArraySlice: UnitViewable where Element == UTF16.CodeUnit {
    public typealias Unit = Element
    public typealias UnitView = Self
}

extension ArraySlice : Content where Element == UTF16.CodeUnit {
}
