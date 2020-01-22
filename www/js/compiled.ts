import { useState, useEffect } from "react";

export function getRustModule(): Promise<RustModule> {
  if (typeof window === "undefined") {
    // return mocks, we're server-rendering.
    return Promise.resolve({
      evalStr: _ => ""
    } as RustModule);
  }

  if (typeof window["Rust"] === "undefined") {
    throw new Error(
      "compiled.ts: failed to load wasm module, window.Rust not defined"
    );
  }

  return window["Rust"].boolean_www.then(
    mod => ({ evalStr: mod.eval_str } as RustModule)
  );
}

// Cache the module for synchronous use
let cachedModule: RustModule | null = null;
getRustModule().then(mod => {
  cachedModule = mod;
});

/**
 * useCompiled() provides the value of the Rust module, asynchronously loaded, into a component.
 *
 * It only loads asynchronously the first time it's used in the application, and is cached for synchronous use after that.
 *
 */
export function useCompiled(): RustModule | null {
  let [compiled, setCompiled] = useState<RustModule | null>(cachedModule);

  useEffect(() => {
    async function load() {
      let mod = await getRustModule();
      setCompiled(mod);
    }
    if (compiled == null) {
      load();
    }
  }, []);

  return compiled;
}

export interface RustModule {
  evalStr: (string) => string;
}
