package combinators

import (
	"fmt"
)

type BoolC bool

type Nat int

type ListC[T any] struct {
	Head T
	Tail *ListC[T]
}

type Btree[T any] interface {
	isBtree()
}

type Bleaf[T any] struct {
	Value T
}

type Bnode[T any] struct {
	Left  Btree[T]
	Right Btree[T]
}

func (b *Bleaf[T]) isBtree() {}
func (b *Bnode[T]) isBtree() {}

type Inftree[T any] struct {
	Left  *Inftree[T]
	Right *Inftree[T]
}

type Inflist[T any] struct {
	Head T
	Tail *Inflist[T]
}

type a b DBtree[a, b] struct {
	Dbleaf a
	Dbnode b (*DBtree[a, b], *DBtree[a, b])
}

type NEtree[T any] interface {
	isNEtree()
}

type Neleaf[T any] struct {
	Value T
}

type Nebranch[T any] struct {
	Value T
	Left  NEtree[T]
	Right NEtree[T]
}

func (n *Neleaf[T]) isNEtree() {}
func (n *Nebranch[T]) isNEtree() {}

type NElist[T any] struct {
	Head T
	Tail *NElist[T]
}

type coListC[T any] struct {
	Head T
	Tail *coListC[T]
}

type coNEtree[T any] interface {
	isCoNEtree()
}

type Nenfone[T any] struct {
	Value T
	Rest   coNEtree[T]
}

type Nefork[T any] struct {
	Left  coNEtree[T]
	Right coNEtree[T]
}

func (n *Nenfone[T]) isCoNEtree() {}
func (n *Nefork[T]) isCoNEtree() {}

type coNElist[T any] struct {
	Head T
	Tail coNElist[T]
}