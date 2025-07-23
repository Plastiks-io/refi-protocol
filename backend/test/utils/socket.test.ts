import { describe, it, expect, beforeEach } from "vitest";
import { initIO, getIO } from "../../src/utils/socket";
import { Server as IOServer } from "socket.io";

const createMockIOServer = () => ({ mock: "io" } as unknown as IOServer);

describe("Socket.IO utility (initIO, getIO)", () => {
  it("should throw an error if getIO is called before initIO", async () => {
    const mod = await import("../../src/utils/socket");

    expect(() => {
      mod.getIO();
    }).toThrowError("Socket.IO not initialized");
  });

  it("should return the same instance that was initialized", () => {
    const mockIO = createMockIOServer();

    initIO(mockIO);
    const result = getIO();

    expect(result).toBe(mockIO);
  });
});
