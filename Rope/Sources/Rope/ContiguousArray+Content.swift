//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
extension ContiguousArray : UnitViewable where Element == UTF16.CodeUnit {
    public typealias Unit = Element
    
    public typealias UnitView = Self
    
}

extension ContiguousArray : Content where Element == UTF16.CodeUnit {
}
