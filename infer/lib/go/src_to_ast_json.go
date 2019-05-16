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

var object_count int64 = 0

var objects map[ast.Node]int64 = make(map[ast.Node]int64)

var func_signatures map[string]string = make(map[string]string)

var json_prefix_len = 0

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
	var sb strings.Builder
	fmt.Println(json_print(&sb, node, *fset, 0))
}

func pad(ind int) string {
	return strings.Repeat(" ", ind)
}

func obj_print(sb *strings.Builder, obj ast.Object, fset token.FileSet, ind int) string {
	var local_sb strings.Builder
	fmt.Fprintf(&local_sb, "%s\"Object\":\n", pad(ind))
	switch n := obj.Decl.(type) {
	case *ast.FuncDecl:
		fmt.Fprintf(&local_sb, json_print(sb, n, fset, ind+indInc))
	case *ast.Field:
		fmt.Fprintf(&local_sb, json_print(sb, n, fset, ind+indInc))
	case *ast.ValueSpec:
		fmt.Fprintf(&local_sb, json_print(sb, n, fset, ind+indInc))
	case *ast.AssignStmt:
		fmt.Fprintf(&local_sb, json_print(sb, n, fset, ind+indInc))
	case *ast.LabeledStmt:
		fmt.Fprintf(&local_sb, json_print(sb, n, fset, ind+indInc))
	case *ast.Object:
		fmt.Fprintf(&local_sb, obj_print(sb, *n, fset, ind))
	default:
		log.Fatal("Unsupported ast object type (for now)")
	}
	return local_sb.String()
}

func ident_print(sb *strings.Builder, id ast.Ident, fset token.FileSet, ind int) string {
	var local_sb strings.Builder
	pos := fset.PositionFor(id.NamePos, true)
	fmt.Fprintf(&local_sb, "%s{\n", pad(ind))
	pref := "\"Ln\":" + strconv.Itoa(pos.Line) + ", \"Col\":" + strconv.Itoa(pos.Column) + ", \"Id\":\"" + id.Name + "\""
	if id.Obj == nil {
		fmt.Fprintf(&local_sb, "%s%s\n", pad(ind+indInc), pref)
	} else {
		fmt.Fprintf(&local_sb, "%s%s,\n", pad(ind+indInc), pref)
		fmt.Fprintf(&local_sb, obj_print(sb, *id.Obj, fset, ind+indInc))
	}
	fmt.Fprintf(&local_sb, "%s}\n", pad(ind))
	return local_sb.String()
}

func block_print(sb *strings.Builder, bs ast.BlockStmt, fset token.FileSet, ind int) string {
	var local_sb strings.Builder
	fmt.Fprintf(&local_sb, "%s{\n", pad(ind))
	lpos := fset.PositionFor(bs.Lbrace, true)
	fmt.Fprintf(&local_sb, "%s\"LBrace\":{\"Ln\":%d, \"Col\":%d},\n",
		pad(ind+indInc), lpos.Line, lpos.Column)
	rpos := fset.PositionFor(bs.Rbrace, true)
	fmt.Fprintf(&local_sb, "%s\"RBrace\":{\"Ln\":%d, \"Col\":%d},\n",
		pad(ind+indInc), rpos.Line, rpos.Column)
	fmt.Fprintf(&local_sb, "%s\"StmtList\":\n", pad(ind+indInc))
	fmt.Fprintf(&local_sb, "%s[\n", pad(ind+indInc))
	for i, s := range bs.List {
		if i > 0 {
			fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
		}
		fmt.Fprintf(&local_sb, json_print(sb, s, fset, ind+2*indInc))
	}
	fmt.Fprintf(&local_sb, "%s]\n", pad(ind+indInc))
	fmt.Fprintf(&local_sb, "%s}\n", pad(ind))
	return local_sb.String()
}

func object_uid(node ast.Node) int64 {
	uid := object_count
	object_count++
	objects[node] = uid
	return uid
}

func json_print(sb *strings.Builder, node ast.Node, fset token.FileSet, ind int) string {
	var local_sb strings.Builder
	switch /*t := */ node.(type) {
	case *ast.File:
		fmt.Fprintln(sb, "{")
		fmt.Fprintln(sb, "\"Defs\":")
		fmt.Fprintln(sb, "[")
		json_prefix_len = sb.Len()

		fmt.Fprintln(&local_sb, "]")
		fmt.Fprintln(&local_sb, ",")
		fmt.Fprintln(&local_sb, "\"Decls\":")
		fmt.Fprintln(&local_sb, "[")
		f, _ := node.(*ast.File)
		for i, d := range f.Decls {
			if i > 0 {
				fmt.Fprintf(&local_sb, "%s,\n", pad(ind+indInc))
			}
			fmt.Fprintf(&local_sb, json_print(sb, d, fset, ind+indInc))
		}
		fmt.Fprintf(sb, local_sb.String())
		fmt.Fprintln(sb, "]")
		fmt.Fprintln(sb, ",")
		fmt.Fprintln(sb, "\"Funcs\":")
		fmt.Fprintln(sb, "[")
		sig_num := len(func_signatures)
		i := 0
		for name, t := range func_signatures {
			fmt.Fprintf(sb, "%s{\n", pad(ind+indInc))
			fmt.Fprintf(sb, "%s\"Name\":\"%s\", \"FuncDesc\":%s", pad(ind+indInc), name, t)
			fmt.Fprintf(sb, "%s}\n", pad(ind+indInc))
			i++
			if i < sig_num {
				fmt.Fprintf(sb, "%s,\n", pad(ind+indInc))
			}
		}
		fmt.Fprintln(sb, "]")

		fmt.Fprintln(sb, "}")
		return sb.String()
	case *ast.FuncDecl:
		uid, found := objects[node]
		if !found {
			fd := node.(*ast.FuncDecl)
			fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
			fmt.Fprintf(&local_sb, "%s\"FuncDecl\",\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
			uid = object_uid(node)
			fmt.Fprintf(&local_sb, "%s\"Uid\":%d, \"Name\":\n", pad(ind+2*indInc), uid)
			fmt.Fprintf(&local_sb, ident_print(sb, *fd.Name, fset, ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s\"FuncDesc\":\n", pad(ind+2*indInc))
			func_desc := json_print(sb, fd.Type, fset, 0)
			fmt.Fprintf(&local_sb, "%s%s", pad(ind+2*indInc), func_desc)
			fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s\"Body\":\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, block_print(sb, *fd.Body, fset, ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
			func_signatures[fd.Name.Name] = func_desc
		} else {
			fmt.Fprintf(&local_sb, "%s[\"FuncDeclRef\", %d]\n", pad(ind), uid)
		}
	case *ast.Ident:
		id := node.(*ast.Ident)
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"Ident\",\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, ident_print(sb, *id, fset, ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.FuncType:
		uid, found := objects[node]
		if !found {
			ind := 2
			ft := node.(*ast.FuncType)
			fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
			fmt.Fprintf(&local_sb, "%s\"FuncType\",\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
			uid = object_uid(node)
			pos := fset.PositionFor(ft.Func, true)
			fmt.Fprintf(&local_sb, "%s\"Uid\":%d, \"Ln\":%d, \"Col\":%d,\n",
				pad(ind+2*indInc), uid, pos.Line, pos.Column)
			fmt.Fprintf(&local_sb, "%s\"Params\":\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, json_print(sb, ft.Params, fset, ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s\"Results\":\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, json_print(sb, ft.Results, fset, ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
			if sb.Len() > json_prefix_len {
				fmt.Fprintf(sb, "%s,\n", pad(ind))
			}
			fmt.Fprintf(sb, local_sb.String())
			local_sb.Reset()
		}
		fmt.Fprintf(&local_sb, "%s[\"FuncTypeRef\", %d]\n", pad(ind+indInc), uid)
	case *ast.FieldList:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fl := node.(*ast.FieldList)
		for i, f := range fl.List {
			if i > 0 {
				fmt.Fprintf(&local_sb, "%s,\n", pad(ind+indInc))
			}
			fmt.Fprintf(&local_sb, json_print(sb, f, fset, ind+2*indInc))
		}
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.Field:
		uid, found := objects[node]
		if !found {
			ind := 2
			f := node.(*ast.Field)
			fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
			fmt.Fprintf(&local_sb, "%s\"Field\",\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
			uid = object_uid(node)
			fmt.Fprintf(&local_sb, "%s\"Uid\":%d, \"Names\":\n", pad(ind+2*indInc), uid)
			fmt.Fprintf(&local_sb, "%s[\n", pad(ind+2*indInc))
			for i, n := range f.Names {
				if i > 0 {
					fmt.Fprintf(&local_sb, "%s,\n", pad(ind+3*indInc))
				}
				fmt.Fprintf(&local_sb, ident_print(sb, *n, fset, ind+3*indInc))
			}
			fmt.Fprintf(&local_sb, "%s]\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s\"Type\":\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, json_print(sb, f.Type, fset, ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
			if sb.Len() > json_prefix_len {
				fmt.Fprintf(sb, "%s,\n", pad(ind))
			}
			fmt.Fprintf(sb, local_sb.String())
			local_sb.Reset()
		}
		fmt.Fprintf(&local_sb, "%s[\"FieldRef\", %d]\n", pad(ind), uid)
	case *ast.StarExpr:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"StarExpr\",\n", pad(ind+indInc))
		se := node.(*ast.StarExpr)
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
		pos := fset.PositionFor(se.Star, true)
		fmt.Fprintf(&local_sb, "%s\"Ln\":%d, \"Col\":%d,\n",
			pad(ind+2*indInc), pos.Line, pos.Column)
		fmt.Fprintf(&local_sb, "%s\"X\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, json_print(sb, se.X, fset, ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.UnaryExpr:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"UnaryExpr\",\n", pad(ind+indInc))
		ue := node.(*ast.UnaryExpr)
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+2*indInc))
		pos := fset.PositionFor(ue.OpPos, true)
		fmt.Fprintf(&local_sb, "%s\"Ln\":%d, \"Col\":%d, \"Op\":\"%s\",\n",
			pad(ind+3*indInc), pos.Line, pos.Column, ue.Op.String())
		fmt.Fprintf(&local_sb, "%s\"X\":\n", pad(ind+3*indInc))
		fmt.Fprintf(&local_sb, json_print(sb, ue.X, fset, ind+4*indInc))
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.BinaryExpr:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"BinaryExpr\",\n", pad(ind+indInc))
		be := node.(*ast.BinaryExpr)
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+2*indInc))
		pos := fset.PositionFor(be.OpPos, true)
		fmt.Fprintf(&local_sb, "%s\"Ln\":%d, \"Col\":%d, \"Op\":\"%s\",\n",
			pad(ind+3*indInc), pos.Line, pos.Column, be.Op.String())
		fmt.Fprintf(&local_sb, "%s\"X\":\n", pad(ind+3*indInc))
		fmt.Fprintf(&local_sb, json_print(sb, be.X, fset, ind+4*indInc))
		fmt.Fprintf(&local_sb, "%s,\n", pad(ind+3*indInc))
		fmt.Fprintf(&local_sb, "%s\"Y\":\n", pad(ind+3*indInc))
		fmt.Fprintf(&local_sb, json_print(sb, be.Y, fset, ind+4*indInc))
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.BlockStmt:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"BlockStmt\",\n", pad(ind+indInc))
		bs := node.(*ast.BlockStmt)
		fmt.Fprintf(&local_sb, block_print(sb, *bs, fset, ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.ReturnStmt:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"ReturnStmt\",\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
		rs := node.(*ast.ReturnStmt)
		pos := fset.PositionFor(rs.Return, true)
		fmt.Fprintf(&local_sb, "%s\"Ln\":%d, \"Col\":%d,\n",
			pad(ind+2*indInc), pos.Line, pos.Column)
		fmt.Fprintf(&local_sb, "%s\"Results\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind+2*indInc))
		for i, e := range rs.Results {
			if i > 0 {
				fmt.Fprintf(&local_sb, "%s,\n", pad(ind+3*indInc))
			}
			fmt.Fprintf(&local_sb, json_print(sb, e, fset, ind+3*indInc))
		}
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.CallExpr:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"CallExpr\",\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
		c := node.(*ast.CallExpr)
		lpos := fset.PositionFor(c.Lparen, true)
		fmt.Fprintf(&local_sb, "%s\"LParen\":{\"Ln\":%d, \"Col\":%d},\n",
			pad(ind+indInc), lpos.Line, lpos.Column)
		rpos := fset.PositionFor(c.Rparen, true)
		fmt.Fprintf(&local_sb, "%s\"RParen\":{\"Ln\":%d, \"Col\":%d},\n",
			pad(ind+indInc), rpos.Line, rpos.Column)
		fmt.Fprintf(&local_sb, "%s\"Fun\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, json_print(sb, c.Fun, fset, ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s\"Args\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind+2*indInc))
		for i, e := range c.Args {
			if i > 0 {
				fmt.Fprintf(&local_sb, "%s,\n", pad(ind+3*indInc))
			}
			fmt.Fprintf(&local_sb, json_print(sb, e, fset, ind+4*indInc))
		}
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.BasicLit:
		bl := node.(*ast.BasicLit)
		pos := fset.PositionFor(bl.ValuePos, true)
		fmt.Fprintf(&local_sb, "%s[\"BasicLit\",{\"Ln\":%d, \"Col\":%d, \"Kind\":\"%s\", \"Value\":\"%s\"}]\n",
			pad(ind), pos.Line, pos.Column, bl.Kind.String(), bl.Value)
	case *ast.AssignStmt:
		uid, found := objects[node]
		if !found {
			as := node.(*ast.AssignStmt)
			fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
			fmt.Fprintf(&local_sb, "%s\"AssignStmt\",\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
			uid = object_uid(node)
			pos := fset.PositionFor(as.TokPos, true)
			fmt.Fprintf(&local_sb, "%s\"Uid\":%d, \"Ln\":%d, \"Col\":%d,\n",
				pad(ind+2*indInc), uid, pos.Line, pos.Column)
			fmt.Fprintf(&local_sb, "%s\"Lhs\":\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s[\n", pad(ind+2+indInc))
			for i, e := range as.Lhs {
				if i > 0 {
					fmt.Fprintf(&local_sb, "%s,\n", pad(ind+3*indInc))
				}
				fmt.Fprintf(&local_sb, json_print(sb, e, fset, ind+3*indInc))
			}
			fmt.Fprintf(&local_sb, "%s]\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s\"Rhs\":\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s[\n", pad(ind+2+indInc))
			for i, e := range as.Rhs {
				if i > 0 {
					fmt.Fprintf(&local_sb, "%s,\n", pad(ind+3*indInc))
				}
				fmt.Fprintf(&local_sb, json_print(sb, e, fset, ind+3*indInc))
			}
			fmt.Fprintf(&local_sb, "%s]\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
		} else {
			fmt.Fprintf(&local_sb, "%s[\"AssignStmtRef\", %d]\n", pad(ind), uid)
		}
	case *ast.DeclStmt:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"DeclStmt\",\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
		ds := node.(*ast.DeclStmt)
		fmt.Fprintf(&local_sb, "%s\"Decl\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, json_print(sb, ds.Decl, fset, ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.GenDecl:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"GenDecl\",\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
		gd := node.(*ast.GenDecl)
		pos := fset.PositionFor(gd.TokPos, true)
		fmt.Fprintf(&local_sb, "%s\"Ln\":%d, \"Col\":%d, \"Tok\":\"%s\",\n",
			pad(ind+2*indInc), pos.Line, pos.Column, gd.Tok.String())
		fmt.Fprintf(&local_sb, "%s\"Specs\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind+2*indInc))
		for i, s := range gd.Specs {
			if i > 0 {
				fmt.Fprintf(&local_sb, "%s,\n", pad(ind+3*indInc))
			}
			fmt.Fprintf(&local_sb, json_print(sb, s, fset, ind+3*indInc))
		}
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.ValueSpec:
		uid, found := objects[node]
		if !found {
			vs := node.(*ast.ValueSpec)
			fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
			fmt.Fprintf(&local_sb, "%s\"ValueSpec\",\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
			uid = object_uid(node)
			fmt.Fprintf(&local_sb, "%s\"Uid\":%d,\n", pad(ind+2*indInc), uid)
			if vs.Type != nil {
				fmt.Fprintf(&local_sb, "%s\"Type\":\n", pad(ind+2*indInc))
				fmt.Fprintf(&local_sb, json_print(sb, vs.Type, fset, ind+2*indInc))
				fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
			}
			fmt.Fprintf(&local_sb, "%s\"Names\":\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s[\n", pad(ind+2*indInc))
			for i, n := range vs.Names {
				if i > 0 {
					fmt.Fprintf(&local_sb, "%s,\n", pad(ind+3*indInc))
				}
				fmt.Fprintf(&local_sb, ident_print(sb, *n, fset, ind+3*indInc))
			}
			fmt.Fprintf(&local_sb, "%s]\n", pad(ind+2*indInc))
			if vs.Values != nil {
				fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
				fmt.Fprintf(&local_sb, "%s\"Values\":\n", pad(ind+2*indInc))
				fmt.Fprintf(&local_sb, "%s[\n", pad(ind+2*indInc))
				for i, v := range vs.Values {
					if i > 0 {
						fmt.Fprintf(&local_sb, "%s,\n", pad(ind+3*indInc))
					}
					fmt.Fprintf(&local_sb, json_print(sb, v, fset, ind+3*indInc))
				}
				fmt.Fprintf(&local_sb, "%s]\n", pad(ind+2*indInc))
			}
			fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
		} else {
			fmt.Fprintf(&local_sb, "%s[\"ValueSpecRef\", %d]\n", pad(ind), uid)
		}
	case *ast.IfStmt:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"IfStmt\",\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
		is := node.(*ast.IfStmt)
		pos := fset.PositionFor(is.If, true)
		fmt.Fprintf(&local_sb, "%s\"Ln\":%d, \"Col\":%d,\n",
			pad(ind+2*indInc), pos.Line, pos.Column)
		fmt.Fprintf(&local_sb, "%s\"Cond\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, json_print(sb, is.Cond, fset, ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s\"Body\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, block_print(sb, *is.Body, fset, ind+2*indInc))
		if is.Else != nil {
			fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s\"Else\":\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, json_print(sb, is.Else, fset, ind+2*indInc))
		}
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.ForStmt:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"ForStmt\",\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
		fs := node.(*ast.ForStmt)
		pos := fset.PositionFor(fs.For, true)
		fmt.Fprintf(&local_sb, "%s\"Ln\":%d, \"Col\":%d,\n",
			pad(ind+2*indInc), pos.Line, pos.Column)
		fmt.Fprintf(&local_sb, "%s\"Init\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, json_print(sb, fs.Init, fset, ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s\"Cond\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, json_print(sb, fs.Cond, fset, ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s\"Post\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, json_print(sb, fs.Post, fset, ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s\"Body\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, block_print(sb, *fs.Body, fset, ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.IncDecStmt:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"IncDecStmt\",\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
		ids := node.(*ast.IncDecStmt)
		pos := fset.PositionFor(ids.TokPos, true)
		fmt.Fprintf(&local_sb, "%s\"Ln\":%d, \"Col\":%d, \"Op\":\"%s\",\n",
			pad(ind+3*indInc), pos.Line, pos.Column, ids.Tok.String())
		fmt.Fprintf(&local_sb, "%s\"X\":\n", pad(ind+2*indInc))
		fmt.Fprintf(&local_sb, json_print(sb, ids.X, fset, ind+2*indInc))
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.BranchStmt:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"BranchStmt\",\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
		bs := node.(*ast.BranchStmt)
		pos := fset.PositionFor(bs.TokPos, true)
		fmt.Fprintf(&local_sb, "%s\"Ln\":%d, \"Col\":%d, \"Keyword\":\"%s\"\n",
			pad(ind+2*indInc), pos.Line, pos.Column, bs.Tok.String())
		if bs.Label != nil {
			fmt.Fprintf(&local_sb, "%s,\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s\"Label\":\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, ident_print(sb, *bs.Label, fset, ind+2*indInc))
		}
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	case *ast.LabeledStmt:
		uid, found := objects[node]
		if !found {
			ls := node.(*ast.LabeledStmt)
			fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
			fmt.Fprintf(&local_sb, "%s\"LabeledStmt\",\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
			uid = object_uid(node)
			pos := fset.PositionFor(ls.Colon, true)
			fmt.Fprintf(&local_sb, "%s\"Uid\":%d, \"Ln\":%d, \"Col\":%d,\n",
				pad(ind+2*indInc), uid, pos.Line, pos.Column)
			fmt.Fprintf(&local_sb, "%s\"Label\":\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, ident_print(sb, *ls.Label, fset, ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s,\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s\"Stmt\":\n", pad(ind+2*indInc))
			fmt.Fprintf(&local_sb, json_print(sb, ls.Stmt, fset, ind+2*indInc))
			fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
			fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
		} else {
			fmt.Fprintf(&local_sb, "%s[\"LabeledStmtRef\", %d]\n", pad(ind), uid)
		}
	case *ast.EmptyStmt:
		fmt.Fprintf(&local_sb, "%s[\n", pad(ind))
		fmt.Fprintf(&local_sb, "%s\"EmptyStmt\",\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s{\n", pad(ind+indInc))
		es := node.(*ast.EmptyStmt)
		pos := fset.PositionFor(es.Semicolon, true)
		fmt.Fprintf(&local_sb, "%s\"Ln\":%d, \"Col\":%d\n",
			pad(ind+2*indInc), pos.Line, pos.Column)
		fmt.Fprintf(&local_sb, "%s}\n", pad(ind+indInc))
		fmt.Fprintf(&local_sb, "%s]\n", pad(ind))
	}
	return local_sb.String()
}
