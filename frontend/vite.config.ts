/// <reference types="vitest/config" />
import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";
import react from "@vitejs/plugin-react-swc";
import { nodePolyfills } from "vite-plugin-node-polyfills";
import path from "path";
import wasm from "vite-plugin-wasm";

// Virtual module plugin to handle stream-browserify/web
const virtualStreamWebPlugin = () => ({
  name: "virtual-stream-web",
  resolveId(id: any) {
    if (id === "stream-browserify/web") {
      return "virtual:stream-web";
    }
    return null;
  },
  load(id: any) {
    if (id === "virtual:stream-web") {
      return `export * from 'web-streams-polyfill';`;
    }
    return null;
  },
});

// https://vite.dev/config/
export default defineConfig({
  plugins: [
    react(),
    tailwindcss(),
    virtualStreamWebPlugin(),
    nodePolyfills({
      globals: {
        Buffer: true,
        global: true,
        process: true,
      },
      // Include all common Node.js modules that lucid-cardano might need
      include: [
        "stream",
        "util",
        "crypto",
        "buffer",
        "net",
        "url",
        "path",
        "fs",
        "zlib",
        "http",
        "https",
      ],
      protocolImports: true,
    }),
    wasm(),
  ],
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "src"),
      stream: "stream-browserify",
      "node:stream": "stream-browserify",
      crypto: "crypto-browserify",
      "node:buffer": "buffer",
      "node:util": "util",
      "node:net": "net-browserify",
      "node:url": "url",
      "node:path": "path-browserify",
      "node:fs": "browserify-fs",
      "node:zlib": "browserify-zlib",
      "node:http": "stream-http",
      "node:https": "https-browserify",
    },
  },
  server: {
    port: 3000,
    open: true,
  },
  define: {
    global: "globalThis",
  },
  build: {
    target: "esnext",
    rollupOptions: {
      external: [
        "worker_threads",
        "node:net",
        "node:http",
        "node:https",
        "node:zlib",
        "node:fs",
        "node:path",
      ],
    },
  },
  optimizeDeps: {
    exclude: ["lucid-cardano"],
    include: [
      "stream-browserify",
      "web-streams-polyfill",
      "crypto-browserify",
      "pbkdf2",
      "buffer",
      "util",
    ],
  },
  test: {
    globals: true,
    environment: "jsdom",
    setupFiles: "./vitest.setup.ts",
  },
});
