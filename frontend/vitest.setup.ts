// src/setupTests.ts
import "@testing-library/jest-dom";
// vitest.setup.ts
import { vi } from "vitest";

// stub global fetch
globalThis.fetch = vi.fn(() =>
  Promise.resolve({
    ok: true,
    json: () =>
      Promise.resolve({
        /* mock payload */
      }),
  })
) as unknown as typeof fetch;
