import { describe, it, expect, vi, beforeAll } from "vitest";

let capturedCalls: any[] = [];

// Fully hoisted-safe mocks
vi.mock("bullmq", () => {
  const internalQueueCalls: any[] = [];

  class MockQueue {
    constructor(name: string, opts: any) {
      internalQueueCalls.push({ name, opts });
    }
  }

  return {
    Queue: MockQueue,
    __esModule: true,
    __vitest__queueCalls: internalQueueCalls,
  };
});

vi.mock("../../src/bull/connection", () => ({
  connection: { redis: "mocked-redis" },
  __esModule: true,
}));

// Import AFTER mocking
import {
  buyNftQueue,
  updateRefiContractQueue,
  updateStakeContractQueue,
} from "../../src/bull/queues";

// Pull captured calls after module loads
beforeAll(async () => {
  const mockedBullmq = await import("bullmq");
  capturedCalls = (mockedBullmq as any).__vitest__queueCalls;
});

describe("BullMQ Queues", () => {
  it("should create buyNftQueue with correct name and connection", () => {
    const match = capturedCalls.find((q) => q.name === "buyNftQueue");
    expect(match).toBeDefined();
    expect(match.opts).toEqual({ connection: { redis: "mocked-redis" } });
    expect(buyNftQueue).toBeDefined();
  });

  it("should create updateRefiContractQueue with correct name and connection", () => {
    const match = capturedCalls.find(
      (q) => q.name === "updateRefiContractQueue"
    );
    expect(match).toBeDefined();
    expect(match.opts).toEqual({ connection: { redis: "mocked-redis" } });
    expect(updateRefiContractQueue).toBeDefined();
  });

  it("should create updateStakeContractQueue with correct name and connection", () => {
    const match = capturedCalls.find(
      (q) => q.name === "updateStakeContractQueue"
    );
    expect(match).toBeDefined();
    expect(match.opts).toEqual({ connection: { redis: "mocked-redis" } });
    expect(updateStakeContractQueue).toBeDefined();
  });

  it("should instantiate exactly 3 queues", () => {
    expect(capturedCalls.length).toBe(3);
  });
});
