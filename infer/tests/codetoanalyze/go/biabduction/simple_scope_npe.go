package empty

func foo(a int) int {

	{
		var p *int = nil
		if a == 7 {
			return *p // error
		}
	}
	{
		var p *int = &a
		return *p
	}

}

func fom(a *int) int {

	var p *int = nil
	if a == nil {
		return *p // error
	} else {
		var b int = 7
		var p *int = &b
		return *p
	}

}

func foz(a *int) int {
	var b int = 7
	var p *int = &b
	if a == nil {
		return *p
	} else {
		var p *int = nil
		return *p // error
	}

}

func fok(a *int, b *int) int {
	var p *int = b
	if a == nil {
		return *p // no error (should it be?)
	} else {
		var b int = 7
		var p *int = &b
		return *p
	}

}

func bar(a int) int {
	var p *int
	var p2 *int = nil
	{
		var i = 42
		var p *int = &i
		p2 = p
	}
	if a == 7 {
		return *p // errror
	} else {
		return *p2
	}
}
