// src/utils/serializers.ts
import Admin from "../models/admin.model.js";
import jwt, { SignOptions } from "jsonwebtoken";
import { Response } from "express";

export function serializeAdmin(admin: Admin) {
  return {
    id: admin.id,
    address: admin.address,
    role: admin.role,
    createdAt: admin.createdAt,
    updatedAt: admin.updatedAt,
  };
}

export interface JWTPayload {
  role: string;
  id?: string;
  email?: string;
}

export function generateJWTToken({ role, id, email }: JWTPayload): string {
  const payload: JWTPayload = { role };
  if (id) payload.id = id;
  if (email) payload.email = email;

  const secret = process.env.JWT_SECRET!;
  const options: SignOptions = { expiresIn: "1d" };
  return jwt.sign(payload, secret, options);
}

/**
 * Generate a Cookie string for the JWT token
 */
export function setAuthCookie(res: Response, token: string): void {
  const cookieName = process.env.JWT_COOKIE_NAME || "token";
  res.cookie(cookieName, token, {
    httpOnly: true,
    secure: process.env.NODE_ENV === "production",
    sameSite: "lax",
    path: "/",
    maxAge: 24 * 60 * 60 * 1000, // 1 day in milliseconds
  });
}
