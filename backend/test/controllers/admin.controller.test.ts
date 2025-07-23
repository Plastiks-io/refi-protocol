import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { Request, Response } from "express";
import {
  createAdmin,
  deleteAdmin,
  getAllAdmins,
  getAdminById,
} from "../../src/controllers/admin.controller";
import Admin, { Role } from "../../src/models/admin.model";
import { serializeAdmin } from "../../src/utils/helper";

// Helper to generate valid mock admin
function createMockAdmin(overrides = {}) {
  return {
    id: "1",
    address: "0x123",
    role: Role.REGULAR,
    createdAt: new Date(),
    updatedAt: new Date(),
    ...overrides,
  };
}

vi.mock("../../src/models/admin.model", () => ({
  default: {
    findOne: vi.fn(),
    create: vi.fn(),
    findByPk: vi.fn(),
    findAll: vi.fn(),
  },
  Role: {
    REGULAR: "REGULAR",
    SUPER: "SUPER",
  },
}));

vi.mock("../../src/utils/helper", () => ({
  serializeAdmin: vi.fn(),
}));

describe("Admin Controller", () => {
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let responseJson: ReturnType<typeof vi.fn>;
  let responseStatus: ReturnType<typeof vi.fn>;

  beforeEach(() => {
    responseJson = vi.fn();
    responseStatus = vi.fn().mockReturnValue({ json: responseJson });

    mockRequest = {};
    mockResponse = {
      status: responseStatus,
      json: responseJson,
    };

    vi.clearAllMocks();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe("createAdmin", () => {
    it("should create a new admin successfully", async () => {
      const mockAdmin = createMockAdmin();
      const serializedAdmin = {
        id: mockAdmin.id,
        address: mockAdmin.address,
        role: mockAdmin.role,
        createdAt: mockAdmin.createdAt,
        updatedAt: mockAdmin.updatedAt,
      };

      mockRequest.body = { address: "0x123" };

      vi.mocked(Admin.findOne).mockResolvedValue(null);
      vi.mocked(Admin.create).mockResolvedValue(mockAdmin as any);
      vi.mocked(serializeAdmin).mockReturnValue(serializedAdmin);

      await createAdmin(mockRequest as Request, mockResponse as Response);

      expect(Admin.findOne).toHaveBeenCalledWith({
        where: { address: "0x123" },
      });
      expect(Admin.create).toHaveBeenCalledWith({
        address: "0x123",
        role: Role.REGULAR,
      });
      expect(serializeAdmin).toHaveBeenCalledWith(mockAdmin);
      expect(responseStatus).toHaveBeenCalledWith(201);
      expect(responseJson).toHaveBeenCalledWith({
        message: "Admin created successfully",
        admin: serializedAdmin,
        success: true,
      });
    });

    it("should return 400 if address is missing", async () => {
      mockRequest.body = {};

      await createAdmin(mockRequest as Request, mockResponse as Response);

      expect(responseStatus).toHaveBeenCalledWith(400);
      expect(responseJson).toHaveBeenCalledWith({
        message: "address is required",
        success: false,
      });
      expect(Admin.findOne).not.toHaveBeenCalled();
    });

    it("should return 400 if admin already exists", async () => {
      const existingAdmin = createMockAdmin();

      mockRequest.body = { address: "0x123" };
      vi.mocked(Admin.findOne).mockResolvedValue(existingAdmin as any);

      await createAdmin(mockRequest as Request, mockResponse as Response);

      expect(Admin.findOne).toHaveBeenCalledWith({
        where: { address: "0x123" },
      });
      expect(responseStatus).toHaveBeenCalledWith(400);
      expect(responseJson).toHaveBeenCalledWith({
        message: "Admin already exists",
        success: false,
      });
      expect(Admin.create).not.toHaveBeenCalled();
    });

    it("should handle database errors", async () => {
      const error = new Error("Database connection failed");

      mockRequest.body = { address: "0x123" };
      vi.mocked(Admin.findOne).mockRejectedValue(error);

      const consoleErrorSpy = vi
        .spyOn(console, "error")
        .mockImplementation(() => {});

      await createAdmin(mockRequest as Request, mockResponse as Response);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        "Error creating admin:",
        error
      );
      expect(responseStatus).toHaveBeenCalledWith(500);
      expect(responseJson).toHaveBeenCalledWith({
        message: "An error occurred while creating admin",
        error: "Database connection failed",
        success: false,
      });

      consoleErrorSpy.mockRestore();
    });
  });

  describe("deleteAdmin", () => {
    it("should delete admin successfully", async () => {
      const mockAdmin = {
        ...createMockAdmin(),
        destroy: vi.fn().mockResolvedValue(undefined),
      };

      mockRequest.params = { id: "1" };
      vi.mocked(Admin.findByPk).mockResolvedValue(mockAdmin as any);

      await deleteAdmin(mockRequest as Request, mockResponse as Response);

      expect(Admin.findByPk).toHaveBeenCalledWith("1");
      expect(mockAdmin.destroy).toHaveBeenCalled();
      expect(responseStatus).toHaveBeenCalledWith(200);
      expect(responseJson).toHaveBeenCalledWith({
        message: "Admin deleted successfully",
        success: true,
      });
    });

    it("should return 404 if admin not found", async () => {
      mockRequest.params = { id: "999" };
      vi.mocked(Admin.findByPk).mockResolvedValue(null);

      await deleteAdmin(mockRequest as Request, mockResponse as Response);

      expect(Admin.findByPk).toHaveBeenCalledWith("999");
      expect(responseStatus).toHaveBeenCalledWith(404);
      expect(responseJson).toHaveBeenCalledWith({
        message: "Admin not found",
        success: false,
      });
    });

    it("should handle deletion errors", async () => {
      const error = new Error("Deletion failed");
      const mockAdmin = {
        ...createMockAdmin(),
        destroy: vi.fn().mockRejectedValue(error),
      };

      mockRequest.params = { id: "1" };
      vi.mocked(Admin.findByPk).mockResolvedValue(mockAdmin as any);

      const consoleErrorSpy = vi
        .spyOn(console, "error")
        .mockImplementation(() => {});

      await deleteAdmin(mockRequest as Request, mockResponse as Response);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        "Error deleting admin:",
        error
      );
      expect(responseStatus).toHaveBeenCalledWith(500);
      expect(responseJson).toHaveBeenCalledWith({
        message: "An error occurred while deleting admin",
        error: "Deletion failed",
        success: false,
      });

      consoleErrorSpy.mockRestore();
    });
  });

  describe("getAllAdmins", () => {
    it("should fetch all admins successfully", async () => {
      const mockAdmins = [
        createMockAdmin({ id: "1" }),
        createMockAdmin({ id: "2", address: "0x456", role: Role.SUPER }),
      ];

      const serializedAdmins = mockAdmins.map((admin) => ({
        id: admin.id,
        address: admin.address,
        role: admin.role,
        createdAt: admin.createdAt,
        updatedAt: admin.updatedAt,
      }));

      vi.mocked(Admin.findAll).mockResolvedValue(mockAdmins as any);
      vi.mocked(serializeAdmin)
        .mockReturnValueOnce(serializedAdmins[0])
        .mockReturnValueOnce(serializedAdmins[1]);

      await getAllAdmins(mockRequest as Request, mockResponse as Response);

      expect(Admin.findAll).toHaveBeenCalled();
      expect(serializeAdmin).toHaveBeenCalledTimes(2);
      expect(responseStatus).toHaveBeenCalledWith(200);
      expect(responseJson).toHaveBeenCalledWith({
        message: "Admins fetched successfully",
        admins: serializedAdmins,
        success: true,
      });
    });

    it("should return empty array when no admins exist", async () => {
      vi.mocked(Admin.findAll).mockResolvedValue([]);

      await getAllAdmins(mockRequest as Request, mockResponse as Response);

      expect(Admin.findAll).toHaveBeenCalled();
      expect(serializeAdmin).not.toHaveBeenCalled();
      expect(responseStatus).toHaveBeenCalledWith(200);
      expect(responseJson).toHaveBeenCalledWith({
        message: "Admins fetched successfully",
        admins: [],
        success: true,
      });
    });

    it("should handle fetch errors", async () => {
      const error = new Error("Database query failed");

      vi.mocked(Admin.findAll).mockRejectedValue(error);

      const consoleErrorSpy = vi
        .spyOn(console, "error")
        .mockImplementation(() => {});

      await getAllAdmins(mockRequest as Request, mockResponse as Response);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        "Error fetching admins:",
        error
      );
      expect(responseStatus).toHaveBeenCalledWith(500);
      expect(responseJson).toHaveBeenCalledWith({
        message: "An error occurred while fetching admins",
        error: "Database query failed",
        success: false,
      });

      consoleErrorSpy.mockRestore();
    });
  });

  describe("getAdminById", () => {
    it("should fetch admin by id successfully", async () => {
      const mockAdmin = createMockAdmin();
      const serializedAdmin = {
        id: mockAdmin.id,
        address: mockAdmin.address,
        role: mockAdmin.role,
        createdAt: mockAdmin.createdAt,
        updatedAt: mockAdmin.updatedAt,
      };

      mockRequest.params = { id: "1" };
      vi.mocked(Admin.findByPk).mockResolvedValue(mockAdmin as any);
      vi.mocked(serializeAdmin).mockReturnValue(serializedAdmin);

      await getAdminById(mockRequest as Request, mockResponse as Response);

      expect(Admin.findByPk).toHaveBeenCalledWith("1");
      expect(serializeAdmin).toHaveBeenCalledWith(mockAdmin);
      expect(responseStatus).toHaveBeenCalledWith(200);
      expect(responseJson).toHaveBeenCalledWith({
        message: "Admin fetched successfully",
        admin: serializedAdmin,
        success: true,
      });
    });

    it("should return 404 if admin not found", async () => {
      mockRequest.params = { id: "999" };
      vi.mocked(Admin.findByPk).mockResolvedValue(null);

      await getAdminById(mockRequest as Request, mockResponse as Response);

      expect(Admin.findByPk).toHaveBeenCalledWith("999");
      expect(responseStatus).toHaveBeenCalledWith(404);
      expect(responseJson).toHaveBeenCalledWith({
        message: "Admin not found",
        success: false,
      });
      expect(serializeAdmin).not.toHaveBeenCalled();
    });

    it("should handle fetch errors", async () => {
      const error = new Error("Database error");

      mockRequest.params = { id: "1" };
      vi.mocked(Admin.findByPk).mockRejectedValue(error);

      const consoleErrorSpy = vi
        .spyOn(console, "error")
        .mockImplementation(() => {});

      await getAdminById(mockRequest as Request, mockResponse as Response);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        "Error fetching admin:",
        error
      );
      expect(responseStatus).toHaveBeenCalledWith(500);
      expect(responseJson).toHaveBeenCalledWith({
        message: "An error occurred while fetching admin",
        error: "Database error",
        success: false,
      });

      consoleErrorSpy.mockRestore();
    });
  });

  describe("Error handling with non-Error objects", () => {
    it("should handle non-Error objects in createAdmin", async () => {
      mockRequest.body = { address: "0x123" };
      vi.mocked(Admin.findOne).mockRejectedValue("String error");

      const consoleErrorSpy = vi
        .spyOn(console, "error")
        .mockImplementation(() => {});

      await createAdmin(mockRequest as Request, mockResponse as Response);

      expect(responseJson).toHaveBeenCalledWith({
        message: "An error occurred while creating admin",
        error: "Unknown error",
        success: false,
      });

      consoleErrorSpy.mockRestore();
    });

    it("should handle non-Error objects in deleteAdmin", async () => {
      mockRequest.params = { id: "1" };
      vi.mocked(Admin.findByPk).mockRejectedValue("String error");

      const consoleErrorSpy = vi
        .spyOn(console, "error")
        .mockImplementation(() => {});

      await deleteAdmin(mockRequest as Request, mockResponse as Response);

      expect(responseJson).toHaveBeenCalledWith({
        message: "An error occurred while deleting admin",
        error: "Unknown error",
        success: false,
      });

      consoleErrorSpy.mockRestore();
    });
  });
});
