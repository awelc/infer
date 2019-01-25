package empty

func foo() int {
	var i int = 42
	var p *int = nil

	if i == 42 {
		return *p // error
	}
	return 7
}

func foz() int {
	var i int = 42
	var p *int = nil

	if i == 42 {
		return *p // error
	} else {
		return 7
	}
}

func fok() int {
	var i int = 42
	var p *int = nil

	if i == 42 {
		return 7
	} else {
		return *p // no error
	}
}

func bar() int {
	var i int = 42
	var p *int = nil

	if i != 42 {
		return 7
	} else {
		return *p // error
	}
}

func baz(a int) int {
	var p1 *int = nil
	var p2 *int = nil

	if a != 42 {
		return *p1 // error
	} else {
		return *p2 // error
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

func bam() int {
	var i int = 42
	var p *int = nil
	var res int = 0

	if i != 42 {
		res = 7
	} else {
		res = *p // error
	}
	return res
}
