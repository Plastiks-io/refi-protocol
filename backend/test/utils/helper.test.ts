import { describe, it, expect, beforeEach, vi } from "vitest";
import {
  serializeAdmin,
  generateJWTToken,
  setAuthCookie,
} from "../../src/utils/helper";
import jwt from "jsonwebtoken";
import type { Response } from "express";

describe("serializeAdmin function", () => {
  it("should serialize Admin object with required fields", () => {
    const mockAdmin = {
      id: "admin123",
      address: "0xabc",
      role: "superadmin",
      createdAt: new Date("2024-01-01"),
      updatedAt: new Date("2024-06-01"),
      password: "should-not-be-included",
    } as any;

    const result = serializeAdmin(mockAdmin);

    expect(result).toEqual({
      id: "admin123",
      address: "0xabc",
      role: "superadmin",
      createdAt: new Date("2024-01-01"),
      updatedAt: new Date("2024-06-01"),
    });

    expect(result).not.toHaveProperty("password");
  });
});

describe("generateJWTToken function", () => {
  it("should generate a JWT with role only", () => {
    process.env.JWT_SECRET = "test-secret";

    const token = generateJWTToken({ role: "admin" });
    const decoded = jwt.verify(token, "test-secret") as any;

    expect(decoded.role).toBe("admin");
    expect(decoded).not.toHaveProperty("id");
    expect(decoded).not.toHaveProperty("email");
  });

  it("should generate a JWT with role, id, and email", () => {
    process.env.JWT_SECRET = "test-secret";

    const token = generateJWTToken({
      role: "user",
      id: "abc123",
      email: "test@example.com",
    });
    const decoded = jwt.verify(token, "test-secret") as any;

    expect(decoded.role).toBe("user");
    expect(decoded.id).toBe("abc123");
    expect(decoded.email).toBe("test@example.com");
  });

  it("should throw if JWT_SECRET is not defined", () => {
    delete process.env.JWT_SECRET;

    expect(() => generateJWTToken({ role: "guest" })).toThrow();
  });
});

describe("setAuthCookie function", () => {
  let res: Response;

  beforeEach(() => {
    res = {
      cookie: vi.fn(),
    } as any;
  });

  it("should set cookie with default name and secure options", () => {
    process.env.NODE_ENV = "production";
    process.env.JWT_COOKIE_NAME = "";

    setAuthCookie(res, "test-token");

    expect(res.cookie).toHaveBeenCalledWith(
      "token",
      "test-token",
      expect.objectContaining({
        httpOnly: true,
        secure: true,
        sameSite: "lax",
        path: "/",
        maxAge: 86400000,
      })
    );
  });

  it("should use custom cookie name from env", () => {
    process.env.NODE_ENV = "development";
    process.env.JWT_COOKIE_NAME = "auth_token";

    setAuthCookie(res, "abc123");

    expect(res.cookie).toHaveBeenCalledWith(
      "auth_token",
      "abc123",
      expect.any(Object)
    );
  });
});
