import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { signIn, signOut } from "../../src/controllers/auth.controller";
import Admin from "../../src/models/admin.model";
import { Request, Response } from "express";
import { generateJWTToken, setAuthCookie } from "../../src/utils/helper";

// Mock Admin model and helper functions
vi.mock("../../src/models/admin.model", () => ({
  default: {
    findOne: vi.fn(),
  },
}));

vi.mock("../../src/utils/helper", () => ({
  generateJWTToken: vi.fn(),
  setAuthCookie: vi.fn(),
}));

describe("Auth Controller", () => {
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let jsonSpy: ReturnType<typeof vi.fn>;
  let statusSpy: ReturnType<typeof vi.fn>;
  let clearCookieSpy: ReturnType<typeof vi.fn>;

  beforeEach(() => {
    jsonSpy = vi.fn();
    statusSpy = vi.fn().mockReturnValue({ json: jsonSpy });
    clearCookieSpy = vi.fn();

    mockRequest = {};
    mockResponse = {
      status: statusSpy,
      json: jsonSpy,
      clearCookie: clearCookieSpy,
    };

    vi.clearAllMocks();
  });

  describe("signIn", () => {
    it("should return 400 if address is missing", async () => {
      mockRequest.body = {};

      await signIn(mockRequest as Request, mockResponse as Response);

      expect(statusSpy).toHaveBeenCalledWith(400);
      expect(jsonSpy).toHaveBeenCalledWith({
        message: "Address is required",
        success: false,
      });
    });

    it("should sign in as USER if no admin found", async () => {
      mockRequest.body = { address: "0xUser" };
      vi.mocked(Admin.findOne).mockResolvedValue(null);
      vi.mocked(generateJWTToken).mockReturnValue("user-token");

      await signIn(mockRequest as Request, mockResponse as Response);

      expect(Admin.findOne).toHaveBeenCalledWith({
        where: { address: "0xUser" },
      });
      expect(generateJWTToken).toHaveBeenCalledWith({ role: "USER" });
      expect(setAuthCookie).toHaveBeenCalledWith(
        mockResponse as Response,
        "user-token"
      );
      expect(statusSpy).toHaveBeenCalledWith(200);
      expect(jsonSpy).toHaveBeenCalledWith({
        message: "User Signed in successfully",
        role: "USER",
        success: true,
      });
    });

    it("should sign in as ADMIN if admin found", async () => {
      const mockAdmin = {
        id: "1",
        address: "0xAdmin",
        role: "SUPER",
      };

      mockRequest.body = { address: "0xAdmin" };
      vi.mocked(Admin.findOne).mockResolvedValue(mockAdmin as any);
      vi.mocked(generateJWTToken).mockReturnValue("admin-token");

      await signIn(mockRequest as Request, mockResponse as Response);

      expect(Admin.findOne).toHaveBeenCalledWith({
        where: { address: "0xAdmin" },
      });
      expect(generateJWTToken).toHaveBeenCalledWith({
        role: "SUPER",
        id: "1",
      });
      expect(setAuthCookie).toHaveBeenCalledWith(
        mockResponse as Response,
        "admin-token"
      );
      expect(statusSpy).toHaveBeenCalledWith(200);
      expect(jsonSpy).toHaveBeenCalledWith({
        message: "Admin signed in successfully",
        role: "SUPER",
        id: "1",
        success: true,
      });
    });

    it("should handle errors gracefully", async () => {
      const error = new Error("DB failed");
      mockRequest.body = { address: "0xError" };
      vi.mocked(Admin.findOne).mockRejectedValue(error);

      const consoleSpy = vi
        .spyOn(console, "error")
        .mockImplementation(() => {});

      await signIn(mockRequest as Request, mockResponse as Response);

      expect(consoleSpy).toHaveBeenCalledWith("Error signing admin in:", error);
      expect(statusSpy).toHaveBeenCalledWith(500);
      expect(jsonSpy).toHaveBeenCalledWith({
        message: "An error occurred while signing admin in",
        error: "DB failed",
        success: false,
      });

      consoleSpy.mockRestore();
    });

    it("should handle non-Error exceptions", async () => {
      mockRequest.body = { address: "0xError" };
      vi.mocked(Admin.findOne).mockRejectedValue("unexpected string");

      const consoleSpy = vi
        .spyOn(console, "error")
        .mockImplementation(() => {});

      await signIn(mockRequest as Request, mockResponse as Response);

      expect(statusSpy).toHaveBeenCalledWith(500);
      expect(jsonSpy).toHaveBeenCalledWith({
        message: "An error occurred while signing admin in",
        error: "Unknown error",
        success: false,
      });

      consoleSpy.mockRestore();
    });
  });

  describe("signOut", () => {
    it("should clear cookie and return success message", async () => {
      process.env.JWT_COOKIE_NAME = "my_token";

      await signOut(mockRequest as Request, mockResponse as Response);

      expect(clearCookieSpy).toHaveBeenCalledWith("my_token", {
        httpOnly: true,
        secure: false,
        sameSite: "lax",
      });
      expect(jsonSpy).toHaveBeenCalledWith({
        message: "Admin signed out successfully",
        success: true,
      });
    });

    it("should use default cookie name when not set", async () => {
      delete process.env.JWT_COOKIE_NAME;

      await signOut(mockRequest as Request, mockResponse as Response);

      expect(clearCookieSpy).toHaveBeenCalledWith("token", {
        httpOnly: true,
        secure: false,
        sameSite: "lax",
      });
    });

    it("should handle errors during sign out", async () => {
      mockResponse.clearCookie = vi.fn(() => {
        throw new Error("Sign out failed");
      });

      const consoleSpy = vi
        .spyOn(console, "error")
        .mockImplementation(() => {});

      await signOut(mockRequest as Request, mockResponse as Response);

      expect(statusSpy).toHaveBeenCalledWith(500);
      expect(jsonSpy).toHaveBeenCalledWith({
        message: "An error occurred while signing admin out",
        error: "Sign out failed",
        success: false,
      });

      consoleSpy.mockRestore();
    });
  });
});
