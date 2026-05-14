import { str, regex, number, sequence, choice, between, lazy, Parser } from '@doeixd/combi-parse';

import { Term } from "./syntax.ts";

// white space consumer
const ws = regex(/\s*/);

const lexeme = <T>(parser: Parser<T>): Parser<T> => parser.keepLeft(ws);

const pKeyword = (s: string) : Parser<string> => lexeme(str(s))

//const parens = <T>(parser: Parser<T>): Parser<T> => between(lexeme(str("(")), parser, lexeme(str(")")));

const pLit: Parser<Term> =
  lexeme(number.map(i => ({ tag: "Lit", value: i }) ));

const pPlus: Parser<Term> =
  sequence([
    pKeyword("+"),
    lazy(() => pTerm),
    lazy(() => pTerm)
    ] as const,
    (([, m, n]) => ({ tag: "Plus", m: m, n: n })));

const pTrue: Parser<Term> =
  pKeyword("true").map(() => ({ tag: "True" }));

const pFalse: Parser<Term> =
  pKeyword("false").map(() => ({ tag: "False" }));

const pIfThenElse: Parser<Term> =
  sequence([
    pKeyword("if"),
    lexeme(lazy(() => pTerm)),
    pKeyword("then"),
    lexeme(lazy(() => pTerm)),
    pKeyword("else"),
    lazy(() => pTerm)
    ] as const,
    (([, m, , n, , p]) => ({ tag: "IfThenElse", m: m, n: n, p: p })));

//const pairLookahead = lookahead(
//  sequence([
//    lexeme(str("(")),
//    lazy(() => pTerm),
//    lexeme(str(","))
//  ] as const, (([]) => null))
//);

//const pPair: Parser<Term> =
//  sequence([
//    pairLookahead,
//    lexeme(str("(")),
//    lazy(() => pTerm),
//    lexeme(str(",")),
//    lazy(() => pTerm),
//    lexeme(str(")")),
//    ] as const,
//    (([, , m , , n,]) => ({ tag: "Pair", m: m, n: n })));

const pFst: Parser<Term> =
  sequence([
    pKeyword("fst"),
    lexeme(lazy(() => pTerm))
   ] as const,
   (([, m]) => ({ tag: "Fst", m: m })));

const pSnd: Parser<Term> =
  sequence([
    pKeyword("snd"),
    lexeme(lazy(() => pTerm))
   ] as const,
   (([, m]) => ({ tag: "Snd", m: m })));

const pParenOrPair: Parser<Term> = lazy(() =>
  lexeme(str("(")).chain(() =>
    lazy(() => pTerm).chain(m =>
      choice([
        // comma → it's a pair
        lexeme(str(",")).chain(() =>
          lazy(() => pTerm).chain(n =>
            lexeme(str(")")).map(() => ({ tag: "Pair", m, n } as Term))
          )
        ),
        // no comma → it's a parenthesized term
        lexeme(str(")")).map(() => m)
      ])
    )
  )
);

const pTerm: Parser<Term> =
    choice([
      pParenOrPair,
      pLit,
      pPlus,
      pTrue,
      pFalse,
      pIfThenElse,
      pFst,
      pSnd
    ]);

export function parseTerm(s: string) {
  return pTerm.parse(s);
}

