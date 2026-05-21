import { parseTerm } from "./parser.ts"
import { prettyPrintTerm } from "./syntax.ts"

const input =
  document.querySelector("#input");

const output =
  document.querySelector("#output");

if (!input) {
  throw new Error("Missing input");
}

if (!output) {
  throw new Error("Missing output");
}

type Hole = { tag: "Hole"; id: int; a: Type | null }

let holeCount = 0;

function makeHole(a: Type | null) {
  let hole = ({tag: "Hole", id: holeCount, a: a});
  holeCount++;
  return hole;
}

function prettyHole(hole: Hole) {
  return `{ }${hole.id}`;
}

type MTerm = Term | Hole

function prettyMTerm(mTerm: MTerm) {
  switch (typeof mTerm.tag) {
    case "Term":
      return prettyTerm(mTerm);
    case "Hole":
      return prettyHole(mTerm);
  }
}

type TermFrame =
  | { tag: "InPlusL"; n: MTerm }
  | { tag: "InPlusR"; m: MTerm }
  | { tag: "InPairL"; n: MTerm }
  | { tag: "InPairR"; n: Mterm }
  | { tag: "InIfScr"; n: MTerm; p: MTerm }
  | { tag: "InIfCon"; m: MTerm; p: MTerm }
  | { tag: "InIfAlt"; n: MTerm; p: MTerm }

function prettyFrame(f: TermFrame, m: string) {
  switch (f.tag) {
    case "InPlusL":
      return `( ${m} + ${prettyMTerm(f.n)} )`;
    case "InPlusR":
      return `( ${prettyMTerm(f.m)} + ${n} )`;
    case "InPlusL":
      return `( ${m}, ${prettyMTerm(f.n)} )`;
    case "InPlusR":
      return `( ${prettyMTerm(f.m)}, ${n} )`;
    case "InIfScr":
      return `( if ${m} then ${prettyMTerm(f.n)} else ${prettyMTerm(f.p)} )`
    case "InIfCon":
      return `( if ${prettyMTerm(f.m)} then ${n} else ${prettyMTerm(f.p)} )`
    case "InIfAlt":
      return `( if ${prettyMTerm(f.m)} then ${prettyMTerm(f.n)} else ${p} )`
  }
}

type TermZipper = { term: MTerm; stack: TermFrame[] }

function prettyZipper(z: TermZipper) {
  function go(stack: TermFrame, m: string) {  
    if (stack.length === 0) {
      return m;
    } else {
      const [head, ...tail] = stack;
      go(tail, prettyFrame(head, m));
    }
  }

  const focus = `▷ ${prettyMTerm(z.term)} ◁`;
  return go(z.stack, focus); 
}

function navUp(z: TermZipper) {
  const [head, ...tail] = z.stack;

  switch (head.tag) {
    case "InPlusL":
      return { term: ({ tag: "Plus", m: z.term, n: head.n }), stack: k };
    case "InPlusR":
      return { term: ({ tag: "Plus", m: head.m, n: z.term }), stack: k };
    case "InPairL":
      return { term: ({ tag: "Pair", m: z.term, n: head.n }), stack: k };
    case "InPairR":
      return { term: ({ tag: "Pair", m: head.m, n: z.term }), stack: k };
    case "InIfScr":
      return { term: ({ tag: "IfThenElse", m: z.term, n: head.n, p: head.p }), stack: k };
    case "InIfCon":
      return { term: ({ tag: "IfThenElse", m: head.m, n: z.term, p: head.p }), stack: k };
    case "InIfAlt":
      return { term: ({ tag: "IfThenElse", m: head.m, n: head.n, p: z.term }), stack: k };
    default:
      return z;
  }
}

function navDown(z: TermZipper) {
  switch (term.tag) {
    case "Plus":
      return { term: term.tag.m, stack: [ ({ tag: "InPlusL", n: term.n }), ...z.stack] };
    case "Pair":
      return { term: term.tag.m, stack: [ ({ tag: "InPairL", n: term.n }), ...z.stack] };
    case "IfThenElse":
      return { term: term.tag.m, stack: [ ({ tag: "InIfScr", n: term.n, p: term.p}), ...z.stack] };
    default:
      return z;
  }
}

function navLeft(z: TermZipper) {
  const [head, ...tail] = z.stack;

  switch (head.tag) {
    case "InPlusR":
      return { term: head.m, stack: [({ tag: "InPlusL", n: z.term }), ...tail] };
    case "InPairR":
      return { term: head.m, stack: [({ tag: "InPairL", n: z.term }), ...tail] };
    case "InIfAlt":
      return { term: head.n, stack: [({ tag: "InIfCon", m: head.m, p: z.term}), ...tail] };
    case "InIfCon":
      return { term: head.m, stack: [({ tag: "InIfScr", n: z.term, p: head.p}), ...tail] };
    default:
      return z;
  }
}

function navRight(z: TermZipper) {
  const [head, ...tail] = z.stack;

  switch (head.tag) {
    case "InPlusL":
      return { term: head.n, stack: [({ tag: "InPlusR", m: z.term }), ...tail] };
    case "InPairL":
      return { term: head.n, stack: [({ tag: "InPairR", m: z.term }), ...tail] };
    case "InIfScr":
      return { term: head.n, stack: [({ tag: "InIfCon", m: z.term, p: head.p}), ...tail] };
    case "InIfCon":
      return { term: head.p, stack: [({ tag: "InIfAlt", m: z.term, n: head.n}), ...tail] };
    default:
      return z;
  }
}

type ZipperState = { focus: TermZipper, hist: TermZipper[] }

const initAst = "if True then fst (+ 1 2, False) else (+ 3 4, True))";

// Initial zipper state
let z = {
  focus: ({
    term: parseTerm(initAst),
    stack: []
  }),
  hist: []
}

input.textContent = prettyZipper(z.focus);

window.addEventListener("keydown", (event) => {
  const curHist = z.hist;

  if (event.key === "ArrowUp") {
    const newState = navUp(z.focus);
    z = ({ focus: newState, hist: curHist});

  } else if (event.key === "ArrowDown") {
    const newState = navDown(z.focus);
    z = ({ focus: newState, hist: curHist});

  } else if (event.key === "ArrowLeft") {
    const newState = navLeft(z.focus);
    z = ({ focus: newState, hist: curHist});

  } else if (event.key === "ArrowRight") {
    const newState = navRight(z.focus);
    z = ({ focus: newState, hist: curHist});
  }
  
  input.textContent = prettyZipper(z.focus);

});

