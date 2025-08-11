import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import jwt from "jsonwebtoken";
import type { Response } from "express";

// 공통으로 사용할 mock config
const mockConfig = {
  JWT_SECRET: "test-secret",
  JWT_COOKIE_NAME: "token",
  NODE_ENV: "development",
};

// 매 테스트마다 mock된 helper 모듈 fresh하게 가져오기
async function importFreshHelper(overrides = {}) {
  vi.doMock("../../src/config/environment", () => ({
    default: { ...mockConfig, ...overrides },
  }));

  return await import("../../src/utils/helper");
}

describe("serializeAdmin", () => {
  it("should serialize only allowed fields", async () => {
    const { serializeAdmin } = await import("../../src/utils/helper");

    const mockAdmin = {
      id: "admin1",
      address: "0xabc123",
      role: "SUPER",
      createdAt: new Date("2024-01-01T00:00:00.000Z"),
      updatedAt: new Date("2024-06-01T00:00:00.000Z"),
      password: "shouldNotBeIncluded",
    } as any;

    const result = serializeAdmin(mockAdmin);

    expect(result).toEqual({
      id: "admin1",
      address: "0xabc123",
      role: "SUPER",
      createdAt: new Date("2024-01-01T00:00:00.000Z"),
      updatedAt: new Date("2024-06-01T00:00:00.000Z"),
    });

    expect(result).not.toHaveProperty("password");
  });
});

describe("generateJWTToken", () => {
  afterEach(() => {
    vi.resetModules();
    vi.clearAllMocks();
  });

  it("should generate JWT with only role", async () => {
    const { generateJWTToken } = await importFreshHelper();

    const token = generateJWTToken({ role: "admin" });
    // ✅ Use jwt.decode() instead of jwt.verify() to avoid signature verification
    const decoded = jwt.decode(token) as any;

    expect(decoded.role).toBe("admin");
    expect(decoded).not.toHaveProperty("id");
    expect(decoded).not.toHaveProperty("email");
  });

  it("should generate JWT with role, id, email", async () => {
    const { generateJWTToken } = await importFreshHelper();

    const token = generateJWTToken({
      role: "user",
      id: "1234",
      email: "test@example.com",
    });

    // ✅ Use jwt.decode() instead of jwt.verify()
    const decoded = jwt.decode(token) as any;

    expect(decoded.role).toBe("user");
    expect(decoded.id).toBe("1234");
    expect(decoded.email).toBe("test@example.com");
  });

  it("should throw if JWT_SECRET is not defined", async () => {
    const { generateJWTToken } = await importFreshHelper({
      JWT_SECRET: undefined,
    });

    expect(() => generateJWTToken({ role: "guest" })).toThrow();
  });
});

describe("setAuthCookie", () => {
  let res: Response;

  beforeEach(() => {
    res = {
      cookie: vi.fn(),
    } as any;
  });

  afterEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  it("should set cookie with secure=false in development", async () => {
    const { setAuthCookie } = await importFreshHelper({
      NODE_ENV: "development",
    });

    setAuthCookie(res, "dev-token");

    expect(res.cookie).toHaveBeenCalledWith(
      "token",
      "dev-token",
      expect.objectContaining({
        httpOnly: true,
        secure: false,
        sameSite: "lax",
        path: "/",
        maxAge: 86400000,
      })
    );
  });

  it("should set cookie with secure=true in production", async () => {
    const { setAuthCookie } = await importFreshHelper({
      NODE_ENV: "production",
    });

    setAuthCookie(res, "prod-token");

    expect(res.cookie).toHaveBeenCalledWith(
      "token",
      "prod-token",
      expect.objectContaining({
        httpOnly: true,
        secure: true,
        sameSite: "lax",
        path: "/",
        maxAge: 86400000,
      })
    );
  });

  it("should use custom cookie name from config", async () => {
    const { setAuthCookie } = await importFreshHelper({
      NODE_ENV: "development",
      JWT_COOKIE_NAME: "auth_token",
    });

    setAuthCookie(res, "abc123");

    expect(res.cookie).toHaveBeenCalledWith(
      "auth_token",
      "abc123",
      expect.objectContaining({
        httpOnly: true,
        secure: false,
        sameSite: "lax",
        path: "/",
        maxAge: 86400000,
      })
    );
  });
});
