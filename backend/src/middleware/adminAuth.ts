// src/middleware/adminAuth.ts

import { Request, Response, NextFunction } from "express";
import jwt, { JwtPayload } from "jsonwebtoken";
import { Role } from "../models/admin.model.js";
import config from "../config/environment.js";

interface AdminToken extends JwtPayload {
  id: string;
  email: string;
  role: Role;
}

// Extend the Express Request interface to include admin property
declare global {
  namespace Express {
    interface Request {
      admin?: AdminToken;
    }
  }
}

export async function adminAuth(
  req: Request,
  res: Response,
  next: NextFunction
) {
  try {
    // Grab the token from cookies (default cookie name: 'token', can be overridden)
    const cookieName = config.JWT_COOKIE_NAME || "token";
    const token = req.cookies?.[cookieName];

    if (!token) {
      res
        .status(401)
        .json({ message: "Missing authentication token in cookies" });
      return;
    }

    const secret = config.JWT_SECRET;
    if (!secret) {
      console.error("JWT_SECRET not set in environment");
      res.status(500).json({ message: "Server configuration error" });
      return;
    }

    // Verify token
    const payload = jwt.verify(token, secret) as AdminToken;

    // Only forbid if it's neither SUPER nor REGULAR
    if (payload.role !== Role.SUPER && payload.role !== Role.REGULAR) {
      res
        .status(403)
        .json({ message: "Forbidden: requires SUPER_ADMIN or ADMIN role" });
      return;
    }

    // Attach to request and proceed
    req.admin = payload;
    next();
  } catch (err) {
    console.error("AdminAuth error:", err);
    res
      .status(401)
      .json({ message: "Invalid or expired authentication token" });
    return;
  }
}
