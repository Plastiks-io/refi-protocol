/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_ADMIN_WALLET_ADDRESS?: string;
  // Add more VITE_ variables here if needed
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
