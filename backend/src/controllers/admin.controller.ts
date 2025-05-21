import { Request, Response } from "express";
import Admin, { Role } from "../models/admin.model.js";
import { serializeAdmin } from "../utils/helper.js";

const createAdmin = async (req: Request, res: Response): Promise<void> => {
  try {
    const { address } = req.body;

    if (!address) {
      res.status(400).json({
        message: "address is required",
        success: false,
      });
      return;
    }

    const existingAdmin = await Admin.findOne({ where: { address } });

    if (existingAdmin) {
      res.status(400).json({
        message: "Admin already exists",
        success: false,
      });
      return;
    }

    const newAdmin = await Admin.create({
      address,
      role: Role.REGULAR,
    });
    res.status(201).json({
      message: "Admin created successfully",
      admin: serializeAdmin(newAdmin),
      success: true,
    });
  } catch (error) {
    console.error("Error creating admin:", error);
    res.status(500).json({
      message: "An error occurred while creating admin",
      error: error instanceof Error ? error.message : "Unknown error",
      success: false,
    });
  }
};

const deleteAdmin = async (req: Request, res: Response): Promise<void> => {
  try {
    const id = req.params.id;

    const adminToDelete = await Admin.findByPk(id);

    if (!adminToDelete) {
      res.status(404).json({
        message: "Admin not found",
        success: false,
      });
      return;
    }

    await adminToDelete.destroy();

    res.status(200).json({
      message: "Admin deleted successfully",
      success: true,
    });
  } catch (error) {
    console.error("Error deleting admin:", error);
    res.status(500).json({
      message: "An error occurred while deleting admin",
      error: error instanceof Error ? error.message : "Unknown error",
      success: false,
    });
  }
};

const getAllAdmins = async (req: Request, res: Response): Promise<void> => {
  try {
    const admins = await Admin.findAll();

    // Only return the fields we want
    const safeAdmins = admins.map(serializeAdmin);

    res.status(200).json({
      message: "Admins fetched successfully",
      admins: safeAdmins,
      success: true,
    });
  } catch (error) {
    console.error("Error fetching admins:", error);
    res.status(500).json({
      message: "An error occurred while fetching admins",
      error: error instanceof Error ? error.message : "Unknown error",
      success: false,
    });
  }
};

const getAdminById = async (req: Request, res: Response): Promise<void> => {
  try {
    const id = req.params.id;
    const admin = await Admin.findByPk(id);

    if (!admin) {
      res.status(404).json({
        message: "Admin not found",
        success: false,
      });
      return;
    }

    // Serialize before sending
    const safeAdmin = serializeAdmin(admin);

    res.status(200).json({
      message: "Admin fetched successfully",
      admin: safeAdmin,
      success: true,
    });
  } catch (error) {
    console.error("Error fetching admin:", error);
    res.status(500).json({
      message: "An error occurred while fetching admin",
      error: error instanceof Error ? error.message : "Unknown error",
      success: false,
    });
  }
};

export { createAdmin, deleteAdmin, getAllAdmins, getAdminById };
