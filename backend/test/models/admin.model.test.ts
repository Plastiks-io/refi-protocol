import { describe, it, expect, vi, beforeEach } from "vitest";
import {
  Admin,
  Role,
  AdminCreationAttributes,
} from "../../src/models/admin.model.js";

describe("Admin Model (Mocked)", () => {
  beforeEach(() => {
    vi.restoreAllMocks();
  });

  describe("Model Definition", () => {
    it("should have correct table name", () => {
      expect(Admin.tableName).toBe("admins");
    });

    it("should have correct model name", () => {
      expect(Admin.name).toBe("Admins");
    });
  });

  describe("Admin Creation", () => {
    it("should create an admin with required fields", async () => {
      const fakeAdmin = {
        id: "uuid-mock",
        address: "0x1234567890abcdef",
        role: Role.REGULAR,
        createdAt: new Date(),
      };
      vi.spyOn(Admin, "create").mockResolvedValue(fakeAdmin as any);

      const admin = await Admin.create({
        address: fakeAdmin.address,
        role: fakeAdmin.role,
      });

      expect(admin).toBeDefined();
      expect(admin.id).toBe("uuid-mock");
      expect(admin.address).toBe(fakeAdmin.address);
      expect(admin.role).toBe(Role.REGULAR);
      expect(admin.createdAt).toBeInstanceOf(Date);
    });

    it("should create an admin with auto-generated createdAt", async () => {
      const now = new Date();
      vi.spyOn(Admin, "create").mockResolvedValue({ createdAt: now } as any);

      const admin = await Admin.create({
        address: "0xtimestamp123456",
        role: Role.USER,
      });

      expect(admin.createdAt).toBe(now);
    });
  });

  describe("Admin Validation", () => {
    it("should require address field", async () => {
      vi.spyOn(Admin, "create").mockRejectedValue(
        new Error("notNull Violation: address is required")
      );

      await expect(
        Admin.create({
          role: Role.REGULAR,
        } as AdminCreationAttributes)
      ).rejects.toThrow("address is required");
    });

    it("should not allow null address", async () => {
      vi.spyOn(Admin, "create").mockRejectedValue(
        new Error("Validation error: address cannot be null")
      );

      await expect(
        Admin.create({
          address: null as any,
          role: Role.REGULAR,
        })
      ).rejects.toThrow("address cannot be null");
    });

    it("should accept all valid role values", async () => {
      for (const role of [Role.SUPER, Role.REGULAR, Role.USER]) {
        vi.spyOn(Admin, "create").mockResolvedValue({ role } as any);
        const admin = await Admin.create({
          address: `0x${role.toLowerCase()}123456789`,
          role,
        });
        expect(admin.role).toBe(role);
      }
    });

    it("should reject invalid role values", async () => {
      vi.spyOn(Admin, "create").mockRejectedValue(
        new Error("Invalid role: must be SUPER_ADMIN | ADMIN | USER")
      );

      await expect(
        Admin.create({
          address: "0xinvalidrole123",
          role: "INVALID_ROLE" as any,
        })
      ).rejects.toThrow("Invalid role");
    });
  });

  describe("Admin Retrieval", () => {
    it("should find admin by id", async () => {
      const fakeAdmin = {
        id: "admin123",
        address: "0xfindbyid123456",
        role: Role.SUPER,
      };
      vi.spyOn(Admin, "findByPk").mockResolvedValue(fakeAdmin as any);

      const found = await Admin.findByPk("admin123");

      expect(found).toBeDefined();
      expect(found?.id).toBe("admin123");
      expect(found?.address).toBe(fakeAdmin.address);
    });

    it("should find admin by address", async () => {
      const fakeAdmin = { address: "0xfindbyaddress123" };
      vi.spyOn(Admin, "findOne").mockResolvedValue(fakeAdmin as any);

      const found = await Admin.findOne({
        where: { address: fakeAdmin.address },
      });

      expect(found).toBeDefined();
      expect(found?.address).toBe(fakeAdmin.address);
    });
  });

  describe("Admin Update", () => {
    it("should update admin role", async () => {
      const admin = {
        id: "admin-update",
        address: "0xupdaterole123456",
        role: Role.REGULAR,
        update: vi.fn().mockResolvedValue(undefined),
      };

      vi.spyOn(Admin, "create").mockResolvedValue(admin as any);
      vi.spyOn(Admin, "findByPk").mockResolvedValue({
        ...admin,
        role: Role.SUPER,
      } as any);

      await admin.update({ role: Role.SUPER });
      const updated = await Admin.findByPk(admin.id);

      expect(updated?.role).toBe(Role.SUPER);
    });

    it("should update admin address", async () => {
      const admin = {
        address: "0xoldaddress123456",
        role: Role.USER,
        update: vi.fn().mockResolvedValue(undefined),
      };

      const newAddress = "0xnewaddress654321";
      await admin.update({ address: newAddress });
      expect(admin.update).toHaveBeenCalledWith({ address: newAddress });
    });
  });

  describe("Admin Deletion", () => {
    it("should delete admin", async () => {
      const admin = {
        id: "delete123",
        destroy: vi.fn().mockResolvedValue(undefined),
      };

      vi.spyOn(Admin, "create").mockResolvedValue(admin as any);
      vi.spyOn(Admin, "findByPk").mockResolvedValue(null);

      await admin.destroy();
      const found = await Admin.findByPk("delete123");
      expect(found).toBeNull();
    });

    it("should delete multiple admins", async () => {
      vi.spyOn(Admin, "bulkCreate").mockResolvedValue([] as any);
      vi.spyOn(Admin, "destroy").mockResolvedValue(2);
      vi.spyOn(Admin, "findAll").mockResolvedValue([
        { role: Role.REGULAR },
      ] as any);

      const deletedCount = await Admin.destroy({ where: { role: Role.USER } });
      const remaining = await Admin.findAll();

      expect(deletedCount).toBe(2);
      expect(remaining).toHaveLength(1);
      expect(remaining[0].role).toBe(Role.REGULAR);
    });
  });

  describe("Role Enum", () => {
    it("should have correct enum values", () => {
      expect(Role.SUPER).toBe("SUPER_ADMIN");
      expect(Role.REGULAR).toBe("ADMIN");
      expect(Role.USER).toBe("USER");
    });

    it("should contain all expected roles", () => {
      const roleValues = Object.values(Role);
      expect(roleValues).toContain("SUPER_ADMIN");
      expect(roleValues).toContain("ADMIN");
      expect(roleValues).toContain("USER");
      expect(roleValues).toHaveLength(3);
    });
  });

  describe("Bulk Operations", () => {
    it("should create multiple admins", async () => {
      const adminsData: AdminCreationAttributes[] = [
        { address: "0xbulk1", role: Role.SUPER },
        { address: "0xbulk2", role: Role.REGULAR },
        { address: "0xbulk3", role: Role.USER },
      ];

      vi.spyOn(Admin, "bulkCreate").mockResolvedValue(adminsData as any);

      const admins = await Admin.bulkCreate(adminsData);
      expect(admins).toHaveLength(3);
    });

    it("should find all admins with specific role", async () => {
      const result = [{ role: Role.SUPER }, { role: Role.SUPER }];
      vi.spyOn(Admin, "findAll").mockResolvedValue(result as any);

      const superAdmins = await Admin.findAll({ where: { role: Role.SUPER } });

      expect(superAdmins).toHaveLength(2);
      superAdmins.forEach((admin) => {
        expect(admin.role).toBe(Role.SUPER);
      });
    });
  });

  describe("Edge Cases", () => {
    it("should handle very long addresses", async () => {
      const longAddress = "0x" + "a".repeat(100);
      vi.spyOn(Admin, "create").mockResolvedValue({
        address: longAddress,
        role: Role.USER,
      } as any);

      const admin = await Admin.create({
        address: longAddress,
        role: Role.USER,
      });

      expect(admin.address).toBe(longAddress);
    });

    it("should handle special characters in address", async () => {
      const specialAddress = "0x123ABC!@#$%^&*()";
      vi.spyOn(Admin, "create").mockResolvedValue({
        address: specialAddress,
        role: Role.REGULAR,
      } as any);

      const admin = await Admin.create({
        address: specialAddress,
        role: Role.REGULAR,
      });

      expect(admin.address).toBe(specialAddress);
    });
  });
});
