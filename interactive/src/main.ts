import { parseTerm } from "./parser.ts"
import { prettyPrintTerm } from "./syntax.ts"

const input =
  document.querySelector<HTMLInputElement>("#initInput");

const output =
  document.querySelector("#output");

if (!input) {
  throw new Error("Missing input");
}

if (!output) {
  throw new Error("Missing output");
}
  

function autoResize(input: HTMLInputElement) {
  function resize() {
    const len = Math.max(input.value.length, 1);
    input.style.width = `${len + 1}ch`;
  }

  resize();

  input.addEventListener("input", resize);
}

type Shed = { id: int; a: Type | null }

type TermFrame =
  | { tag: "LitF"; i: number }
  | { tag: "BoolF"; b: boolean }
  | { tag: "FstF"; m: TermFrame; a: Type | Null }
  | { tag: "SndF"; m: TermFrame; a: Type | Null }
  | { tag: "PlusF"; m: TermFrame; n: TermFrame; a: Type | Null }
  | { tag: "PairF"; m: TermFrame; n: TermFrame; a: Type | Null }
  | { tag: "IfThenElseF"; m: TermFrame; n: TermFrame; a: Type | Null }
  | { tag: "ShedF"; shed: Shed };

let shedCount = 0;

function makeShed(a: Type | null) {
  let shed = ({id: shedCount, type: a});
  shedCount++;
  return shed;
}

function toShedF(shed: Shed) {
  return ({ tag: "ShedF", shed: shed });
}

function printShed(shed: Shed) {
  return `{ }${shed.id}`;
}

const frameStack: TermFrame[] = []

function frameStackToText(stack: TermFrame[]) {
}

function insertText(
  input: HTMLInputElement,
  text: string,
  cursorOffset = text.length
) {
  const start = input.selectionStart ?? 0;
  const end = input.selectionEnd ?? 0;

  input.value =
    input.value.slice(0, start) +
    text +
    input.value.slice(end);

  const pos = start + cursorOffset;

  input.setSelectionRange(pos, pos);

  try {
    let m = parseTerm(input.value);
    output.textContent = prettyPrintTerm(m);
  } catch (ParseError) {
    output.textContent = "Error";
  } finally {
    autoResize(input);
  }
}

autoResize(input);


input.addEventListener("keydown", (event) => {
  if (event.key === "(") {
    event.preventDefault();

    insertText(input, "()", 1);
  } else if (event.key === "+") {
    event.preventDefault();
    let s1 = toShedF(makeShed(({ tag: "Number" })));
    let s2 = toShedF(makeShed(({ tag: "Number" })));
    let f = ({ tag: "PlusF", m: s1, n: s1, a: ({ tag: "Number" })});
    let s1Str = printShed(s1.shed);
    let s2Str = printShed(s2.shed);
    let offset = s1Str.length + s2Str.length;
    frameStack.push(f);
    frameStack.push(s1);
    insertText(input,`+ ${s1Str} ${s2Str}`, offset);

    

  } else {
    try {
      let m = parseTerm(input.value);
      output.textContent = prettyPrintTerm(m);
    } catch (ParseError) {
      output.textContent = "Error";
    }
  }

});

