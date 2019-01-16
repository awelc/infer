package empty

func foo() int {
	var a int = 42
	var p *int = nil
	for i := 0; i < 1; i++ {
		a = *p // error
	}
	return a
}

func foz() int {
	var a int = 42
	var p *int = nil
	for i := 0; i < 0; i++ {
		a = *p // no error
	}
	return a
}

func bar() int {
	var a int = 42
	var p *int = nil
	for i := 0; i < 1; i++ {
		return a
		a = *p // surprisingly, this is legal (no error)
	}
	return a
}
