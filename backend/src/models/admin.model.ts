// src/models/admin.model.ts

import { Model, DataTypes, Optional } from "sequelize";
import sequelize from "../db/config.js";

// 1. List all columns in your table
export interface AdminAttributes {
  id: string;
  address: string;
  role: Role;
  createdAt: Date;
}

// 2. For `.create()` and `.build()`, which fields are optional?
//    id is auto-generated
export interface AdminCreationAttributes
  extends Optional<AdminAttributes, "id" | "createdAt"> {}

// 3. Extend Model with those two interfaces:
export class Admin
  extends Model<AdminAttributes, AdminCreationAttributes>
  implements AdminAttributes
{
  // Declare attributes without creating public class fields
  declare id: string;
  declare address: string;
  declare role: Role;

  // Timestamps
  declare readonly createdAt: Date;
  declare readonly updatedAt: Date;
}

export enum Role {
  SUPER = "SUPER_ADMIN",
  REGULAR = "ADMIN",
  USER = "USER",
}

// 4. Initialize your model
Admin.init(
  {
    id: {
      type: DataTypes.UUID,
      defaultValue: DataTypes.UUIDV4,
      allowNull: false,
      primaryKey: true,
    },
    address: {
      type: DataTypes.STRING,
      allowNull: false,
    },
    role: {
      type: DataTypes.ENUM(...Object.values(Role)),
      allowNull: false,
      defaultValue: Role.REGULAR,
    },
    createdAt: {
      type: DataTypes.DATE,
      defaultValue: DataTypes.NOW,
    },
  },
  {
    sequelize,
    modelName: "Admins",
    tableName: "admins",
    timestamps: false,
  }
);

export default Admin;
