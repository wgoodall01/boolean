import * as React from "react";
import { useEffect, useState } from "react";
import { useCompiled } from "../js/compiled";
import Repl from "../components/Repl";

export default function App() {
  const compiled = useCompiled();
  return (
    <div className="container">
      <style jsx>{`
        .container {
          max-width: 50rem;
          margin: 2rem;
        }
      `}</style>
      <h1>boolean repl</h1>
      {compiled == null ? <pre>loading...</pre> : <Repl />}
    </div>
  );
}
