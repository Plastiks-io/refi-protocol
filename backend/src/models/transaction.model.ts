// src/models/transaction.model.ts

import { Model, DataTypes, Optional } from "sequelize";
import sequelize from "../db/config.js";

// Enum for transaction type
export enum TransactionType {
  Sold = "creditSale",
  Transfer = "fundTransfer",
}

// Interface for all table attributes
export interface TransactionAttributes {
  id: string;
  txDate: Date;
  txFee: number;
  amount: number;
  assetId: string;
  hash: string;
  type: TransactionType;
  createdAt: Date;
}

// Optional fields when creating
export interface TransactionCreationAttributes
  extends Optional<TransactionAttributes, "id" | "createdAt"> {}

// Sequelize model
export class Transaction
  extends Model<TransactionAttributes, TransactionCreationAttributes>
  implements TransactionAttributes
{
  declare id: string;
  declare txDate: Date;
  declare txFee: number;
  declare amount: number;
  declare assetId: string;
  declare hash: string;
  declare type: TransactionType;
  declare createdAt: Date;
}

// Initialize model
Transaction.init(
  {
    id: {
      type: DataTypes.UUID,
      defaultValue: DataTypes.UUIDV4,
      primaryKey: true,
    },
    txDate: {
      type: DataTypes.DATE,
      allowNull: false,
    },
    txFee: {
      type: DataTypes.FLOAT,
      allowNull: false,
    },
    amount: {
      type: DataTypes.FLOAT,
      allowNull: false,
    },
    assetId: {
      type: DataTypes.STRING,
      allowNull: false,
    },
    hash: {
      type: DataTypes.STRING,
      allowNull: false,
    },
    type: {
      type: DataTypes.ENUM(...Object.values(TransactionType)),
      allowNull: false,
    },
    createdAt: {
      type: DataTypes.DATE,
      defaultValue: DataTypes.NOW,
    },
  },
  {
    sequelize,
    modelName: "Transaction",
    tableName: "transaction",
    timestamps: false,
  }
);
