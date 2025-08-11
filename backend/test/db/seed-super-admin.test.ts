import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { Admin, Role } from "../../src/models/admin.model";

// Vitest module mocking: inline the mock object directly
vi.mock("../../src/config/environment", () => ({
  default: {
    NODE_ENV: "testing",
    JWT_SECRET: "test_jwt_secret",
    DATABASE: {
      HOST: "localhost",
      PORT: 5432,
      USER: "test_user",
      PASSWORD: "test_password",
      NAME: "test_db",
    },
    WALLETS: {
      ADMIN_WALLET_ADDRESS: "addr_test1custom_address_here",
      ADMIN_SEED: "some-admin-seed",
      ADMIN_PKH: "some-admin-pkh",
      NOZAMA_ADDRESS: "some-nozama",
      PC_WALLET: "some-pc-wallet",
      DEAD_WALLET_ADDRESS: "some-dead-wallet",
    },
    JWT_COOKIE_NAME: "token",
    SUPER_ADMIN: {
      EMAIL: "test@example.com",
      PASSWORD: "password",
      ADDRESS: "",
    },
  },
}));

describe("seedSuperAdmin", () => {
  let mockAdmin;
  let findOneSpy;
  let createSpy;
  let consoleLogSpy;
  let seedSuperAdmin;

  const defaultAddress = "addr_test1custom_address_here";

  beforeEach(async () => {
    // Dynamically import after mocking so the module uses the mocked config
    const seedModule = await import("../../src/db/seed-super-admin");
    seedSuperAdmin = seedModule.seedSuperAdmin;

    consoleLogSpy = vi.spyOn(console, "log").mockImplementation(() => {});
    findOneSpy = vi.spyOn(Admin, "findOne");
    createSpy = vi.spyOn(Admin, "create");
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe("when no super admin exists", () => {
    beforeEach(() => {
      mockAdmin = {
        id: 1,
        role: Role.SUPER,
        address: defaultAddress,
        save: vi.fn().mockResolvedValue(undefined),
      };
      findOneSpy.mockResolvedValue(null);
      createSpy.mockResolvedValue(mockAdmin);
    });

    it("creates a new super admin with default address when config var is not set", async () => {
      // Reset mock to simulate config without SUPER_ADMIN_ADDRESS
      vi.doMock("../../src/config/environment", () => ({
        default: {
          NODE_ENV: "testing",
          JWT_SECRET: "test_jwt_secret",
          DATABASE: {
            HOST: "localhost",
            PORT: 5432,
            USER: "test_user",
            PASSWORD: "test_password",
            NAME: "test_db",
          },
          SUPER_ADMIN_ADDRESS: "", // empty simulates missing config
          JWT_COOKIE_NAME: "token",
        },
      }));
      // Re-import seedSuperAdmin module to use updated config mock
      const { seedSuperAdmin: seed } = await import(
        "../../src/db/seed-super-admin"
      );

      await seed();

      expect(findOneSpy).toHaveBeenCalledWith({
        where: { address: defaultAddress },
      });
      expect(createSpy).toHaveBeenCalledWith({
        role: Role.SUPER,
        address: defaultAddress,
      });
      expect(consoleLogSpy).not.toHaveBeenCalled();
    });

    it("creates a new super admin with custom address from config", async () => {
      // Normally mock is already with a custom address
      await seedSuperAdmin();

      expect(findOneSpy).toHaveBeenCalledWith({
        where: { address: "addr_test1custom_address_here" },
      });
      expect(createSpy).toHaveBeenCalledWith({
        role: Role.SUPER,
        address: "addr_test1custom_address_here",
      });
      expect(consoleLogSpy).not.toHaveBeenCalled();
    });

    it("handles empty string config by using default address", async () => {
      // Repeat the empty config test
      vi.doMock("../../src/config/environment", () => ({
        default: {
          NODE_ENV: "testing",
          JWT_SECRET: "test_jwt_secret",
          DATABASE: {
            HOST: "localhost",
            PORT: 5432,
            USER: "test_user",
            PASSWORD: "test_password",
            NAME: "test_db",
          },
          SUPER_ADMIN_ADDRESS: "",
          JWT_COOKIE_NAME: "token",
        },
      }));
      const { seedSuperAdmin: seed } = await import(
        "../../src/db/seed-super-admin"
      );
      await seed();

      expect(findOneSpy).toHaveBeenCalledWith({
        where: { address: defaultAddress },
      });
      expect(createSpy).toHaveBeenCalledWith({
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
        save: vi.fn().mockResolvedValue(undefined),
      };
      findOneSpy.mockResolvedValue(mockAdmin);
    });

    it("skips seeding and logs message when super admin exists", async () => {
      await seedSuperAdmin();

      expect(findOneSpy).toHaveBeenCalledWith({
        where: { address: defaultAddress },
      });
      expect(createSpy).not.toHaveBeenCalled();
      expect(mockAdmin.save).not.toHaveBeenCalled();
      expect(consoleLogSpy).toHaveBeenCalledWith(
        "Super admin already exists, skipping seeding"
      );
    });

    it("uses custom address from config when checking existing admin", async () => {
      mockAdmin.address = "addr_test1custom_address_here";
      findOneSpy.mockResolvedValue(mockAdmin);

      await seedSuperAdmin();

      expect(findOneSpy).toHaveBeenCalledWith({
        where: { address: "addr_test1custom_address_here" },
      });
      expect(consoleLogSpy).toHaveBeenCalledWith(
        "Super admin already exists, skipping seeding"
      );
    });
  });

  describe("error handling", () => {
    it("propagates database errors from findOne", async () => {
      const dbError = new Error("Database connection failed");
      findOneSpy.mockRejectedValue(dbError);

      await expect(seedSuperAdmin()).rejects.toThrow(
        "Database connection failed"
      );
    });

    it("propagates database errors from create", async () => {
      findOneSpy.mockResolvedValue(null);
      const dbError = new Error("Failed to create admin");
      createSpy.mockRejectedValue(dbError);

      await expect(seedSuperAdmin()).rejects.toThrow("Failed to create admin");
    });

    it("propagates database errors from save", async () => {
      mockAdmin = {
        id: 1,
        role: "ADMIN",
        address: defaultAddress,
        save: vi.fn(),
      };
      findOneSpy.mockResolvedValue(mockAdmin);
      const dbError = new Error("Failed to save admin");
      mockAdmin.save.mockRejectedValue(dbError);

      await expect(seedSuperAdmin()).rejects.toThrow("Failed to save admin");
    });
  });
});
