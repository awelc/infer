/*
package main

import "fmt"

func main() {
	fmt.Println(foo())
}
*/

package empty

func foo(i int) int {
	var p *int = nil
	if i == 7 {
		goto label
	}
	return 42
label:
	return *p // errror
}

func foz() int {
	var p *int = nil
	for i := 0; i < 7; i++ {
		continue
		return *p // no error
	}
	return 42
}

func fok() int {
	var p *int = nil
	for i := 0; i < 7; i++ {
		if i < 7 {
			continue
		}
		return *p // no error
	}
	return 42
}

func bar() int {
	var p *int = nil
	for i := 0; i < 7; i++ {
		if i < 1 {
			continue
		}
		return *p // error
	}
	return 42
}

func baz() int {
	var p *int = nil
	for i := 0; i < 7; i++ {
		break
		return *p // no error
	}
	return 42
}

func bak() int {
	var p *int = nil
	for i := 0; i < 7; i++ {
		if i > 7 {
			break
		}
		return *p // error (should it be?)
	}
	return 42
}

func bam() int {
	var p *int = nil
	for i := 0; i < 7; i++ {
		if i > 1 {
			break
		}
		return *p // error
	}
	return 42
}
