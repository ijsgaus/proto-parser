namespace ProtoParser

type Ident = internal | Ident of string

type FullIdent = internal | FullIdent of NameSpace : Ident list * Name : Ident

type MessageType = internal | MessageType of bool * FullIdent
type EnumType = internal | EnumType of bool * FullIdent
