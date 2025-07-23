import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { seedSuperAdmin } from "../../src/db/seed-super-admin";
import { Admin, Role } from "../../src/models/admin.model";
// Mock dependencies
vi.mock("../models/admin.model.js");
vi.mock("dotenv");

describe("seedSuperAdmin", () => {
  let mockAdmin;
  let mockAdminFindOne;
  let mockAdminCreate;
  let mockAdminSave;
  let consoleSpy;
  let originalEnv;

  const defaultAddress =
    "addr_test1qregzqux7knjhg3v8npcp3t35w0dngwkz80ssgvywpk0ade9uy5qk6vl70ntchwh6qysnlww6q28vsjd6sz8kpdq2w0skcj8zp";

  beforeEach(() => {
    // Store original env
    originalEnv = process.env;

    // Mock console.log
    consoleSpy = vi.spyOn(console, "log").mockImplementation(() => {});

    // Mock Admin model methods
    mockAdminFindOne = vi.fn();
    mockAdminCreate = vi.fn();
    mockAdminSave = vi.fn();

    // Mock Admin model
    vi.mocked(Admin).findOne = mockAdminFindOne;
    vi.mocked(Admin).create = mockAdminCreate;

    // Mock Role enum
    vi.mocked(Role).SUPER = Role.SUPER;

    // Clear modules to ensure fresh imports
    vi.clearAllMocks();
  });

  afterEach(() => {
    // Restore original env
    process.env = originalEnv;
    consoleSpy.mockRestore();
  });

  describe("when no super admin exists", () => {
    beforeEach(() => {
      mockAdminFindOne.mockResolvedValue(null);
      mockAdmin = {
        id: 1,
        role: Role.SUPER,
        address: defaultAddress,
        save: mockAdminSave,
      };
      mockAdminCreate.mockResolvedValue(mockAdmin);
    });

    it("should create a new super admin with default address when env var is not set", async () => {
      delete process.env.SUPER_ADMIN_ADDRESS;

      await seedSuperAdmin();

      expect(mockAdminFindOne).toHaveBeenCalledWith({
        where: { address: defaultAddress },
      });
      expect(mockAdminCreate).toHaveBeenCalledWith({
        role: Role.SUPER,
        address: defaultAddress,
      });
      expect(consoleSpy).not.toHaveBeenCalled();
    });

    it("should create a new super admin with custom address from env var", async () => {
      const customAddress = "addr_test1custom_address_here";
      process.env.SUPER_ADMIN_ADDRESS = customAddress;

      await seedSuperAdmin();

      expect(mockAdminFindOne).toHaveBeenCalledWith({
        where: { address: customAddress },
      });
      expect(mockAdminCreate).toHaveBeenCalledWith({
        role: Role.SUPER,
        address: customAddress,
      });
      expect(consoleSpy).not.toHaveBeenCalled();
    });

    it("should handle empty string env var by using default address", async () => {
      process.env.SUPER_ADMIN_ADDRESS = "";

      await seedSuperAdmin();

      expect(mockAdminFindOne).toHaveBeenCalledWith({
        where: { address: defaultAddress },
      });
      expect(mockAdminCreate).toHaveBeenCalledWith({
        role: Role.SUPER,
        address: defaultAddress,
      });
    });
  });

  describe("when super admin exists with correct role", () => {
    beforeEach(() => {
      mockAdmin = {
        id: 1,
        role: Role.SUPER,
        address: defaultAddress,
        save: mockAdminSave,
      };
      mockAdminFindOne.mockResolvedValue(mockAdmin);
    });

    it("should skip seeding and log message when super admin already exists with SUPER role", async () => {
      await seedSuperAdmin();

      expect(mockAdminFindOne).toHaveBeenCalledWith({
        where: { address: defaultAddress },
      });
      expect(mockAdminCreate).not.toHaveBeenCalled();
      expect(mockAdminSave).not.toHaveBeenCalled();
      expect(consoleSpy).toHaveBeenCalledWith(
        "Super admin already exists, skipping seeding"
      );
    });

    it("should use custom address from env var when checking existing admin", async () => {
      const customAddress = "addr_test1custom_existing_address";
      process.env.SUPER_ADMIN_ADDRESS = customAddress;

      mockAdmin.address = customAddress;
      mockAdminFindOne.mockResolvedValue(mockAdmin);

      await seedSuperAdmin();

      expect(mockAdminFindOne).toHaveBeenCalledWith({
        where: { address: customAddress },
      });
      expect(consoleSpy).toHaveBeenCalledWith(
        "Super admin already exists, skipping seeding"
      );
    });
  });

  describe("error handling", () => {
    it("should propagate database errors from findOne", async () => {
      const dbError = new Error("Database connection failed");
      mockAdminFindOne.mockRejectedValue(dbError);

      await expect(seedSuperAdmin()).rejects.toThrow(
        "Database connection failed"
      );
    });

    it("should propagate database errors from create", async () => {
      mockAdminFindOne.mockResolvedValue(null);
      const dbError = new Error("Failed to create admin");
      mockAdminCreate.mockRejectedValue(dbError);

      await expect(seedSuperAdmin()).rejects.toThrow("Failed to create admin");
    });

    it("should propagate database errors from save", async () => {
      mockAdmin = {
        id: 1,
        role: "ADMIN",
        address: defaultAddress,
        save: mockAdminSave,
      };
      mockAdminFindOne.mockResolvedValue(mockAdmin);
      const dbError = new Error("Failed to save admin");
      mockAdminSave.mockRejectedValue(dbError);

      await expect(seedSuperAdmin()).rejects.toThrow("Failed to save admin");
    });
  });
});
