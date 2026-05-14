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
  }

}

autoResize(input);

input.addEventListener("keydown", (event) => {
  if (event.key === "(") {
    event.preventDefault();

    insertText(input, "()", 1);
  } else {
    try {
      let m = parseTerm(input.value);
      output.textContent = prettyPrintTerm(m);
    } catch (ParseError) {
      output.textContent = "Error";
    }
  }

});

