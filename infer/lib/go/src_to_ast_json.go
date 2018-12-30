package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"
	"os"
	"strconv"
	"strings"
)

const indInc = 2

func main() {
	args := os.Args
	if len(args) != 3 {
		fmt.Println("USAGE: " + args[0] + " file_name")
		os.Exit(0)
	}
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, args[2], nil, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}
	json_print(node, *fset, 0)

}

func pad(ind int) string {
	return strings.Repeat(" ", ind)
}

func ident_obj(id ast.Ident, fset token.FileSet) string {
	pos := fset.PositionFor(id.NamePos, true)
	return "{\"Ln\":" + strconv.Itoa(pos.Line) + ", \"Col\":" + strconv.Itoa(pos.Column) + ", \"Id\":\"" + id.Name + "\"}"
}

func json_print(node ast.Node, fset token.FileSet, ind int) {
	switch /*t := */ node.(type) {
	case *ast.File:
		fmt.Println("{")
		fmt.Println("\"Decls\":")
		fmt.Println("[")
		f, _ := node.(*ast.File)
		for i, d := range f.Decls {
			if i > 0 {
				fmt.Printf("%s,\n", pad(ind+indInc))
			}
			json_print(d, fset, ind+2*indInc)
		}
		fmt.Println("]")
		fmt.Println("}")
	case *ast.FuncDecl:
		fmt.Printf("%s[\n", pad(ind))
		fmt.Printf("%s\"FuncDecl\",\n", pad(ind+indInc))
		fmt.Printf("%s{\n", pad(ind+indInc))
		fd, _ := node.(*ast.FuncDecl)
		fmt.Printf("%s\"Name\":%s\n", pad(ind+2*indInc), ident_obj(*fd.Name, fset))
		fmt.Printf("%s,\n", pad(ind+2*indInc))
		json_print(fd.Type, fset, ind+2*indInc)
		fmt.Printf("%s,\n", pad(ind+2*indInc))
		fmt.Printf("%s\"Body\":\n", pad(ind+2*indInc))
		json_print(fd.Body, fset, ind+2*indInc)
		fmt.Printf("%s}\n", pad(ind+indInc))
		fmt.Printf("%s]\n", pad(ind))
	case *ast.Ident:
		id, _ := node.(*ast.Ident)
		fmt.Printf("%s[\"Ident\", %s]\n", pad(ind), ident_obj(*id, fset))
	case *ast.FuncType:
		fmt.Printf("%s\"FuncType\":\n", pad(ind))
		fmt.Printf("%s{\n", pad(ind))
		ft, _ := node.(*ast.FuncType)
		pos := fset.PositionFor(ft.Func, true)
		fmt.Printf("%s\"Ln\":%d, \"Col\":%d,\n",
			pad(ind+indInc), pos.Line, pos.Column)
		fmt.Printf("%s\"Params\":\n", pad(ind+indInc))
		json_print(ft.Params, fset, ind+indInc)
		fmt.Printf("%s,\n", pad(ind+indInc))
		fmt.Printf("%s\"Results\":\n", pad(ind+indInc))
		json_print(ft.Results, fset, ind+indInc)
		fmt.Printf("%s}\n", pad(ind))
	case *ast.FieldList:
		fmt.Printf("%s[\n", pad(ind))
		fl, _ := node.(*ast.FieldList)
		for i, f := range fl.List {
			if i > 0 {
				fmt.Printf("%s,\n", pad(ind+indInc))
			}
			json_print(f, fset, ind+2*indInc)
		}
		fmt.Printf("%s]\n", pad(ind))
	case *ast.Field:
		fmt.Printf("%s{\n", pad(ind))
		fmt.Printf("%s\"Names\":\n", pad(ind+indInc))
		fmt.Printf("%s[\n", pad(ind+indInc))
		f, _ := node.(*ast.Field)
		for i, n := range f.Names {
			if i > 0 {
				fmt.Printf("%s,\n", pad(ind+2*indInc))
			}
			fmt.Printf("%s%s\n", pad(ind+2*indInc), ident_obj(*n, fset))
		}
		fmt.Printf("%s]\n", pad(ind+indInc))
		fmt.Printf("%s,\n", pad(ind+indInc))
		fmt.Printf("%s\"Type\":\n", pad(ind+indInc))
		json_print(f.Type, fset, ind+2*indInc)
		fmt.Printf("%s}\n", pad(ind))
	case *ast.StarExpr:
		fmt.Printf("%s[\n", pad(ind))
		fmt.Printf("%s\"StarExpr\",\n", pad(ind+indInc))
		se, _ := node.(*ast.StarExpr)
		fmt.Printf("%s{\n", pad(ind+2*indInc))
		pos := fset.PositionFor(se.Star, true)
		fmt.Printf("%s\"Ln\":%d, \"Col\":%d,\n",
			pad(ind+3*indInc), pos.Line, pos.Column)
		fmt.Printf("%s\"X\":\n", pad(ind+3*indInc))
		json_print(se.X, fset, ind+4*indInc)
		fmt.Printf("%s}\n", pad(ind+2*indInc))
		fmt.Printf("%s]\n", pad(ind))
	case *ast.UnaryExpr:
		fmt.Printf("%s[\n", pad(ind))
		fmt.Printf("%s\"UnaryExpr\",\n", pad(ind+indInc))
		ue, _ := node.(*ast.UnaryExpr)
		fmt.Printf("%s{\n", pad(ind+2*indInc))
		pos := fset.PositionFor(ue.OpPos, true)
		fmt.Printf("%s\"Ln\":%d, \"Col\":%d, \"Tok\":\"%s\",\n",
			pad(ind+3*indInc), pos.Line, pos.Column, ue.Op.String())
		fmt.Printf("%s\"X\":\n", pad(ind+3*indInc))
		json_print(ue.X, fset, ind+4*indInc)
		fmt.Printf("%s}\n", pad(ind+2*indInc))
		fmt.Printf("%s]\n", pad(ind))
	case *ast.BlockStmt:
		fmt.Printf("%s{\n", pad(ind))
		bs, _ := node.(*ast.BlockStmt)
		lpos := fset.PositionFor(bs.Lbrace, true)
		fmt.Printf("%s\"LBrace\":{\"Ln\":%d, \"Col\":%d},\n",
			pad(ind+indInc), lpos.Line, lpos.Column)
		rpos := fset.PositionFor(bs.Rbrace, true)
		fmt.Printf("%s\"RBrace\":{\"Ln\":%d, \"Col\":%d},\n",
			pad(ind+indInc), rpos.Line, rpos.Column)
		fmt.Printf("%s\"StmtList\":\n", pad(ind+indInc))
		fmt.Printf("%s[\n", pad(ind+indInc))
		for i, s := range bs.List {
			if i > 0 {
				fmt.Printf("%s,\n", pad(ind+2*indInc))
			}
			json_print(s, fset, ind+2*indInc)
		}
		fmt.Printf("%s]\n", pad(ind+indInc))
		fmt.Printf("%s}\n", pad(ind))
	case *ast.ReturnStmt:
		fmt.Printf("%s[\n", pad(ind))
		fmt.Printf("%s\"ReturnStmt\",\n", pad(ind+indInc))
		fmt.Printf("%s{\n", pad(ind+indInc))
		rs, _ := node.(*ast.ReturnStmt)
		pos := fset.PositionFor(rs.Return, true)
		fmt.Printf("%s\"Ln\":%d, \"Col\":%d,\n",
			pad(ind+2*indInc), pos.Line, pos.Column)
		fmt.Printf("%s\"Results\":\n", pad(ind+2*indInc))
		fmt.Printf("%s[\n", pad(ind+2*indInc))
		for i, e := range rs.Results {
			if i > 0 {
				fmt.Printf("%s,\n", pad(ind+3*indInc))
			}
			json_print(e, fset, ind+3*indInc)
		}
		fmt.Printf("%s]\n", pad(ind+2*indInc))
		fmt.Printf("%s}\n", pad(ind+indInc))
		fmt.Printf("%s]\n", pad(ind))
	case *ast.CallExpr:
		fmt.Printf("%s\"CallExpr\":\n", pad(ind))
		fmt.Printf("%s{\n", pad(ind))
		c, _ := node.(*ast.CallExpr)
		json_print(c.Fun, fset, ind+indInc)
		fmt.Printf("%s,\n", pad(ind+indInc))
		fmt.Printf("%s\"Args\":\n", pad(ind+indInc))
		fmt.Printf("%s[\n", pad(ind+indInc))
		for i, e := range c.Args {
			if i > 0 {
				fmt.Printf("%s,\n", pad(ind+2*indInc))
			}
			fmt.Printf("%s{\n", pad(ind+2*indInc))
			json_print(e, fset, ind+3*indInc)
			fmt.Printf("%s}\n", pad(ind+2*indInc))
		}
		fmt.Printf("%s]\n", pad(ind+indInc))
		fmt.Printf("%s}\n", pad(ind))
	case *ast.BasicLit:
		bl, _ := node.(*ast.BasicLit)
		pos := fset.PositionFor(bl.ValuePos, true)
		fmt.Printf("%s[\"BasicLit\",{\"Ln\":%d, \"Col\":%d, \"Kind\":\"%s\", \"Value\":\"%s\"}]\n",
			pad(ind), pos.Line, pos.Column, bl.Kind.String(), bl.Value)
	case *ast.AssignStmt:
		fmt.Printf("%s[\n", pad(ind))
		fmt.Printf("%s\"AssignStmt\",\n", pad(ind+indInc))
		fmt.Printf("%s{\n", pad(ind+indInc))
		as, _ := node.(*ast.AssignStmt)
		pos := fset.PositionFor(as.TokPos, true)
		fmt.Printf("%s\"Ln\":%d, \"Col\":%d,\n",
			pad(ind+2*indInc), pos.Line, pos.Column)
		fmt.Printf("%s\"Lhs\":\n", pad(ind+2*indInc))
		fmt.Printf("%s[\n", pad(ind*2+indInc))
		for i, e := range as.Lhs {
			if i > 0 {
				fmt.Printf("%s,\n", pad(ind+3*indInc))
			}
			json_print(e, fset, ind+3*indInc)
		}
		fmt.Printf("%s]\n", pad(ind+2*indInc))
		fmt.Printf("%s,\n", pad(ind+2*indInc))
		fmt.Printf("%s\"Rhs\":\n", pad(ind+2*indInc))
		fmt.Printf("%s[\n", pad(ind*2+indInc))
		for i, e := range as.Rhs {
			if i > 0 {
				fmt.Printf("%s,\n", pad(ind+3*indInc))
			}
			json_print(e, fset, ind+3*indInc)
		}
		fmt.Printf("%s]\n", pad(ind+2*indInc))
		fmt.Printf("%s}\n", pad(ind+indInc))
		fmt.Printf("%s]\n", pad(ind))
	case *ast.DeclStmt:
		fmt.Printf("%s[\n", pad(ind))
		fmt.Printf("%s\"DeclStmt\",\n", pad(ind+indInc))
		fmt.Printf("%s{\n", pad(ind+indInc))
		ds, _ := node.(*ast.DeclStmt)
		fmt.Printf("%s\"Decl\":\n", pad(ind+2*indInc))
		json_print(ds.Decl, fset, ind+2*indInc)
		fmt.Printf("%s}\n", pad(ind+indInc))
		fmt.Printf("%s]\n", pad(ind))
	case *ast.GenDecl:
		fmt.Printf("%s[\n", pad(ind))
		fmt.Printf("%s\"GenDecl\",\n", pad(ind+indInc))
		fmt.Printf("%s{\n", pad(ind+indInc))
		gd, _ := node.(*ast.GenDecl)
		pos := fset.PositionFor(gd.TokPos, true)
		fmt.Printf("%s\"Ln\":%d, \"Col\":%d, \"Tok\":\"%s\",\n",
			pad(ind+2*indInc), pos.Line, pos.Column, gd.Tok.String())
		fmt.Printf("%s\"Specs\":\n", pad(ind+2*indInc))
		fmt.Printf("%s[\n", pad(ind+2*indInc))
		for i, s := range gd.Specs {
			if i > 0 {
				fmt.Printf("%s,\n", pad(ind+3*indInc))
			}
			json_print(s, fset, ind+3*indInc)
		}
		fmt.Printf("%s]\n", pad(ind+2*indInc))
		fmt.Printf("%s}\n", pad(ind+indInc))
		fmt.Printf("%s]\n", pad(ind))
	case *ast.ValueSpec:
		fmt.Printf("%s[\n", pad(ind))
		fmt.Printf("%s\"ValueSpec\",\n", pad(ind+indInc))
		fmt.Printf("%s{\n", pad(ind+indInc))
		vs, _ := node.(*ast.ValueSpec)
		fmt.Printf("%s\"Type\":\n", pad(ind+2*indInc))
		json_print(vs.Type, fset, ind+2*indInc)
		fmt.Printf("%s,\n", pad(ind+2*indInc))
		fmt.Printf("%s\"Names\":\n", pad(ind+2*indInc))
		fmt.Printf("%s[\n", pad(ind+2*indInc))
		for i, n := range vs.Names {
			if i > 0 {
				fmt.Printf("%s,\n", pad(ind+3*indInc))
			}
			fmt.Printf("%s%s\n", pad(ind+3*indInc), ident_obj(*n, fset))
		}
		fmt.Printf("%s]\n", pad(ind+2*indInc))
		fmt.Printf("%s,\n", pad(ind+2*indInc))
		fmt.Printf("%s\"Values\":\n", pad(ind+2*indInc))
		fmt.Printf("%s[\n", pad(ind+2*indInc))
		for i, v := range vs.Values {
			if i > 0 {
				fmt.Printf("%s,\n", pad(ind+3*indInc))
			}
			json_print(v, fset, ind+3*indInc)
		}
		fmt.Printf("%s]\n", pad(ind+2*indInc))
		fmt.Printf("%s}\n", pad(ind+indInc))
		fmt.Printf("%s]\n", pad(ind))
	}

}
