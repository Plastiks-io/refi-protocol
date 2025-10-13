import * as CardanoWasm from "@emurgo/cardano-serialization-lib-browser";

// Attach so libraries that expect a global binding can find it
(globalThis as any).Cardano = CardanoWasm;
// some environments/lucid builds look for cardanoSerializationLib too:
(globalThis as any).cardanoSerializationLib = CardanoWasm;
// import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import "./index.css";
import { Provider } from "react-redux";
import { store } from "./redux/store";
import App from "./App.tsx";

createRoot(document.getElementById("root")!).render(
  <Provider store={store}>
    <App />
  </Provider>
);
