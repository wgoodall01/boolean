import * as React from "react";
import { useState } from "react";
import { useCompiled } from "../js/compiled";
import Input from "./Input";

interface Cell {
  input: string;
  output?: string;
}

export default function Repl() {
  const compiled = useCompiled();
  const [cells, setCells] = useState<Cell[]>([{ input: "" }]);
  const [focusInd, setFocusInd] = useState<number>(0);

  // TODO: ripout
  if (typeof window !== "undefined") {
    // @ts-ignore
    window.setFocusInd = setFocusInd;
  }

  if (compiled == null) {
    return <pre>repl loading...</pre>;
  }

  function updateCell(ind: number, fn: (Cell) => Cell): Cell[] {
    let newCells = [...cells];
    newCells[ind] = fn(newCells[ind]);
    return newCells;
  }

  const changeHandler = (ind: number) => (expr: string) =>
    setCells(updateCell(ind, c => ({ ...c, input: expr })));

  const submitHandler = (ind: number) => (expr: string) => {
    // Evaluate the indexed cell
    let cells = updateCell(ind, c => ({
      ...c,
      output: compiled.evalStr(c.input.split("#")[0])
    }));

    // Ignore empty cells.
    if (cells[ind].input == "") {
      return;
    }

    // If we're the last cell, append one
    if (ind == cells.length - 1) {
      cells.push({ input: "" });
    }

    setCells(cells);
    setFocusInd(e => e + 1); // Increment the focus index
  };

  return (
    <div>
      <style jsx>{`
        .cell {
          margin-bottom: 1rem;
        }

        .output {
          padding: 0.5rem;
          width: 100%;
          display: block;
        }
      `}</style>
      {cells.map((cell, ind) => (
        <div key={ind} className="cell">
          <Input
            value={cells[ind].input}
            onChange={changeHandler(ind)}
            onSubmit={submitHandler(ind)}
            focus={ind == focusInd}
            onFocus={() => setFocusInd(ind)}
            alwaysShowExamples={ind == cells.length - 1}
          />
          {cell.output == null ? null : (
            <div className="output">{cell.output}</div>
          )}
        </div>
      ))}
    </div>
  );
}
