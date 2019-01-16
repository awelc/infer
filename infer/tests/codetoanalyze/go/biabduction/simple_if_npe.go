package empty

func foo() int {
	var i int = 42
	var p *int = nil

	if i == 42 {
		return *p
	}
	return 7
}

func foz() int {
	var i int = 42
	var p *int = nil

	if i == 42 {
		return *p
	} else {
		return 7
	}
}

func bar() int {
	var i int = 42
	var p *int = nil

	if i != 42 {
		return 7
	} else {
		return *p
	}
}

func baz(a int) int {
	var p1 *int = nil
	var p2 *int = nil

	if a != 42 {
		return *p1
	} else {
		return *p2
	}
}

func bak() int {
	var i int = 42
	var p *int = nil

	if i == 42 {
		return i
	} else {
		return 7
		var a int = *p // surprisingly, this is legal (no error)
		return a
	}
}
