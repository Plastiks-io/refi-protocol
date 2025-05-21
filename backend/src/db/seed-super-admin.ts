// src/db/seed-super-admin.ts
import bcrypt from "bcrypt";
import { Admin, Role } from "../models/admin.model.js";
import dotenv from "dotenv";
dotenv.config();

export async function seedSuperAdmin() {
  const address =
    process.env.SUPER_ADMIN_ADDRESS ||
    "addr_test1qregzqux7knjhg3v8npcp3t35w0dngwkz80ssgvywpk0ade9uy5qk6vl70ntchwh6qysnlww6q28vsjd6sz8kpdq2w0skcj8zp";

  let superAdmin = await Admin.findOne({ where: { address } });
  if (!superAdmin) {
    superAdmin = await Admin.create({
      role: Role.SUPER,
      address,
    });
  } else if (superAdmin.role !== Role.SUPER) {
    superAdmin.role = Role.SUPER;
    await superAdmin.save();
  } else {
    console.log("Super admin already exists, skipping seeding");
  }
}
