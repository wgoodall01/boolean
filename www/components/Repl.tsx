import * as React from "react";
import { useState } from "react";
import { useCompiled } from "../js/compiled";

interface Cell {
  input: string;
  output?: string;
}

export default function Repl() {
  const compiled = useCompiled();
  const [cells, setCells] = useState<Cell[]>([{ input: "" }]);

  if (compiled == null) {
    return <pre>repl loading...</pre>;
  }

  function updateCell(ind: number, fn: (Cell) => Cell): Cell[] {
    let newCells = [...cells];
    newCells[ind] = fn(newCells[ind]);
    return newCells;
  }

  const changeHandler = (ind: number) => (
    e: React.ChangeEvent<HTMLInputElement>
  ) => setCells(updateCell(ind, c => ({ ...c, input: e.target.value })));

  const keyPressHandler = (ind: number) => (
    e: React.KeyboardEvent<HTMLInputElement>
  ) => {
    if (e.key == "Enter") {
      // Evaluate the indexed cell
      let cells = updateCell(ind, c => ({
        ...c,
        output: compiled.evalStr(c.input)
      }));

      // If we're the last cell, append one
      if (ind == cells.length - 1 && cells[ind].input !== "") {
        cells.push({ input: "" });
      }

      setCells(cells);
    }
  };

  return (
    <div>
      <style jsx>{`
        .cell {
          margin-bottom: 1rem;
        }

        .input {
          margin-bottom: 0.5rem;
          padding: 0;
          border: 0;
          font-weight: bold;
          background-color: rgba(0, 0, 0, 0.05);
        }

        .input:focus {
          background-color: black;
          color: white;
        }

        .output,
        .input {
          padding: 0.5rem;
          width: 100%;
          display: block;
        }
      `}</style>
      {cells.map((cell, ind) => (
        <div key={ind} className="cell">
          <input
            type="text"
            onChange={changeHandler(ind)}
            onKeyPress={keyPressHandler(ind)}
            className="input"
            autoFocus
          />
          {cell.output == null ? null : (
            <div className="output">{cell.output}</div>
          )}
        </div>
      ))}
    </div>
  );
}
