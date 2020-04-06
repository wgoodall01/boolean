import * as React from "react";
import { useEffect, useRef } from "react";

const autocompletes = `
Table expr var1 ... varN
Satisfy expr
With expr assumptions
Debug expr
Inert expr
`.trim();

const examples = `
(a & b) | a                 # Simplify an expression
Table (a & (b | c))         # Generate truth tables in disjunctive normal form
With (a | b & c) (a & !b)   # Substitute in concrete assumptions
Satisfy (a & (b | c))       # Find values which satisfy the expression
Satisfy (Table (a | b & c)) # Combine expressions
Debug (a & b | c & d)       # Print the parsed AST
`.trim();

interface Props {
  value: string;
  onChange: (string) => any;
  onSubmit?: (string) => any;
  alwaysShowExamples?: boolean;
  focus?: boolean;
  onFocus: () => any;
}

export default function Input({
  value,
  onChange = () => {},
  onSubmit = () => {},
  focus = false,
  onFocus = () => {},
  alwaysShowExamples = true
}: Props) {
  const inputRef = useRef(null);

  useEffect(() => {
    if (focus) {
      inputRef.current.focus();
    } else {
      inputRef.current.blur();
    }
  }, [focus]);

  const handleChange = e => onChange(e.target.value);

  const handleKeyPress = e => {
    if (e.key == "Enter") {
      onSubmit(e.target.value);
    }
  };

  return (
    <>
      <style jsx>{`
        input {
          margin-bottom: 0.5rem;
          padding: 0;
          border: 0;
          font-weight: bold;
          background-color: rgba(0, 0, 0, 0.05);
          padding: 0.5rem;
          width: 100%;
          display: block;
        }

        .attachment {
          margin: 0.5rem
          padding-bottom: 0;
          color: rgba(0,0,0,0.3);
        }

        input:focus {
          background-color: black;
          color: white;
        }
      `}</style>

      <input
        type="text"
        onChange={handleChange}
        onKeyPress={handleKeyPress}
        value={value}
        ref={inputRef}
        onFocus={() => onFocus()}
      />

      {focus && value != "" && (
        <pre className="attachment">
          {autocompletes
            .split("\n")
            .filter(e => e.startsWith(value))
            .join("\n")}
        </pre>
      )}
      {((focus && value == "") || alwaysShowExamples) && (
        <pre className="attachment">{examples}</pre>
      )}
    </>
  );
}
