import Admin from "../models/admin.model.js";
import { Request, Response } from "express";
import { generateJWTToken, setAuthCookie } from "../utils/helper.js";

const signIn = async (req: Request, res: Response): Promise<void> => {
  try {
    const { address } = req.body;
    if (!address) {
      res.status(400).json({
        message: "Address is required",
        success: false,
      });
      return;
    }

    // 1. Find admin by address
    const admin = await Admin.findOne({ where: { address } });
    // If no admin found that means it is user trying to sign in
    if (!admin) {
      const token = generateJWTToken({ role: "USER" });
      setAuthCookie(res, token);
      res.status(200).json({
        message: "User Signed in successfully",
        role: "USER",
        success: true,
      });
      return;
    }

    // If admin found, it means it is admin trying to sign in
    // 2. Create JWT
    const token = generateJWTToken({
      role: admin.role,
      id: admin.id,
    });

    // 3. Set cookie
    setAuthCookie(res, token);

    // 4. Respond
    res.status(200).json({
      message: "Admin signed in successfully",
      role: admin.role,
      id: admin.id,
      success: true,
    });
    return;
  } catch (error) {
    console.error("Error signing admin in:", error);
    res.status(500).json({
      message: "An error occurred while signing admin in",
      error: error instanceof Error ? error.message : "Unknown error",
      success: false,
    });
  }
};

const signOut = async (req: Request, res: Response): Promise<void> => {
  try {
    const cookieName = process.env.JWT_COOKIE_NAME || "token";

    // Clear the cookie
    res.clearCookie(cookieName, {
      httpOnly: true,
      secure: process.env.NODE_ENV === "production",
      sameSite: "lax",
    });

    res.json({
      message: "Admin signed out successfully",
      success: true,
    });
  } catch (error) {
    console.error("Error signing admin out:", error);
    res.status(500).json({
      message: "An error occurred while signing admin out",
      error: error instanceof Error ? error.message : "Unknown error",
      success: false,
    });
  }
};

export { signIn, signOut };
