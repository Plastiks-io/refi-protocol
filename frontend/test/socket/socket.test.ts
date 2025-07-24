// test/socket.test.ts
import { describe, it, expect, beforeEach, vi, afterEach } from "vitest";
import { io, Socket } from "socket.io-client";

vi.mock("socket.io-client", async () => {
  return {
    io: vi.fn(() => ({
      connected: true,
      io: { opts: {} },
    })),
  };
});

describe("Socket Initialization", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    vi.resetModules();
  });

  afterEach(() => {
    process.env = originalEnv;
    vi.clearAllMocks();
  });

  it("should connect using VITE_SERVER_URL if defined", async () => {
    process.env = {
      ...originalEnv,
      VITE_SERVER_URL: "https://custom.server.com",
    };

    const { socket } = await import("../../src/socket/socket");

    expect(io).toHaveBeenCalledWith("https://custom.server.com", {
      withCredentials: true,
    });

    expect(socket.connected).toBe(true);
  });

  it("should fallback to localhost if VITE_SERVER_URL is undefined", async () => {
    process.env = {
      ...originalEnv,
      VITE_SERVER_URL: undefined as any,
    };

    const { socket } = await import("../../src/socket/socket");

    expect(io).toHaveBeenCalledWith("http://localhost:8080", {
      withCredentials: true,
    });

    expect(socket.connected).toBe(true);
  });

  it("should always set withCredentials to true", async () => {
    process.env = {
      ...originalEnv,
      VITE_SERVER_URL: "https://myserver.test",
    };

    const { socket } = await import("../../src/socket/socket");

    expect(io).toHaveBeenCalled();
    const callArgs = (io as any).mock.calls[0][1];

    expect(callArgs.withCredentials).toBe(true);
  });

  it("should return a valid socket object", async () => {
    const { socket } = await import("../../src/socket/socket");
    expect(socket).toBeDefined();
    expect(typeof socket).toBe("object");
    expect("connected" in socket).toBe(true);
  });
});
