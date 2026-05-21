export type Type =
  | { tag: "Number" }
  | { tag: "Boolean" }
  | { tag: "Prod"; a: Type; b: Type };

export type Term =
  | { tag: "Lit"; value: number }
  | { tag: "Plus"; m: Term; n: Term }
  | { tag: "True" }
  | { tag: "False" }
  | { tag: "IfThenElse"; m: Term; n: Term; p: Term }
  | { tag: "Pair"; m: Term; n: Term }
  | { tag: "Fst"; m: Term; }
  | { tag: "Snd"; m: Term };


export function prettyTerm(ast: Term) {
  switch (ast.tag) {
    case "Lit":
      return ast.value.toString();
    
    case "True":
      return "true";

    case "False":
      return "false";

    case "Fst":
      return `(fst ${prettyPrintTerm(ast.m)})`;
    
    case "Snd":
      return `(snd ${prettyPrintTerm(ast.m)})`;

    case "Plus":
      return `(+ ${prettyPrintTerm(ast.m)} ${prettyPrintTerm(ast.n)})`;

    case "Pair":
      return `(${prettyPrintTerm(ast.m)}, ${prettyPrintTerm(ast.n)})`;

    case "IfThenElse":
      return `(if ${prettyPrintTerm(ast.m)}\n
               \tthen ${prettyPrintTerm(ast.n)}\n
               \telse ${prettyPrintTerm(ast.p)})`;
  }
}


