//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
public protocol Initializable {
	associatedtype Initializer
	init(_ initial: Initializer)
}

