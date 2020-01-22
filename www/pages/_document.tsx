import Document, { Html, Head, Main, NextScript } from "next/document";
import { resolve } from "path";
import { existsSync, readlinkSync } from "fs";

class MyDocument extends Document {
  static async getInitialProps(ctx) {
    const initialProps = await Document.getInitialProps(ctx);
    return { ...initialProps };
  }

  render() {
    return (
      <Html>
        <Head />
        <body>
          <style>{`
            html {
              font-family: sans-serif;
            }

          `}</style>
          <Main />
          <script src={"/boolean-www.js"}></script>
          <NextScript />
        </body>
      </Html>
    );
  }
}

export default MyDocument;
