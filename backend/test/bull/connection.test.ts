import { describe, it, expect, vi, beforeEach } from "vitest";

describe("Redis Connection", () => {
  let mockRedisConstructor: ReturnType<typeof vi.fn>;

  beforeEach(async () => {
    mockRedisConstructor = vi.fn();

    vi.doMock("ioredis", () => ({
      Redis: mockRedisConstructor,
    }));

    // Re-import after mocking
    await import("../../src/bull/connection");
  });

  it("should initialize Redis with correct configuration", async () => {
    expect(mockRedisConstructor).toHaveBeenCalledTimes(1);
    expect(mockRedisConstructor).toHaveBeenCalledWith({
      host: "127.0.0.1",
      port: 6379,
      maxRetriesPerRequest: null,
      enableReadyCheck: false,
    });
  });
});
