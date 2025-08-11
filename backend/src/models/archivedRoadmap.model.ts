// src/models/archivedRoadmap.model.ts

import { Model, DataTypes, Optional, DecimalDataType } from "sequelize";
import sequelize from "../db/config.js";

// 1. List all columns in your table
export interface ArchivedRoadmapAttributes {
  id: string;
  preId: string;
  roadmapId: string;
  roadmapName: string;
  roadmapDescription: string;
  progress: DecimalDataType;
  preAddress: string;
  totalPlasticCredits: number;
  soldPlasticCredits: number;
  totalPlasticTokens: number;
  sentPlasticTokens: number;
  totalPlastic: number;
  recoveredPlastic: number;
  createdAt: Date;
  dateArchived: Date;
  status: string;
}

// 2. For `.create()` and `.build()`, which fields are optional?
//    id, createdAt, dateArchived, and status have default values
export interface ArchivedRoadmapCreationAttributes
  extends Optional<
    ArchivedRoadmapAttributes,
    "id" | "createdAt" | "dateArchived" | "status"
  > {}

// 3. Extend Model with those two interfaces
export class ArchivedRoadmap
  extends Model<ArchivedRoadmapAttributes, ArchivedRoadmapCreationAttributes>
  implements ArchivedRoadmapAttributes
{
  // Declare attributes without creating public class fields
  declare id: string;
  declare preId: string;
  declare roadmapId: string;
  declare roadmapName: string;
  declare roadmapDescription: string;
  declare progress: DecimalDataType;
  declare preAddress: string;
  declare totalPlasticCredits: number;
  declare soldPlasticCredits: number;
  declare totalPlasticTokens: number;
  declare sentPlasticTokens: number;
  declare totalPlastic: number;
  declare recoveredPlastic: number;
  declare readonly createdAt: Date;
  declare readonly dateArchived: Date;
  declare status: string;
}

// 4. Initialize your model
ArchivedRoadmap.init(
  {
    id: {
      type: DataTypes.UUID,
      defaultValue: DataTypes.UUIDV4,
      allowNull: false,
      primaryKey: true,
    },
    preId: {
      type: DataTypes.STRING,
      allowNull: false,
    },
    roadmapId: {
      type: DataTypes.STRING,
      allowNull: false,
    },
    roadmapName: {
      type: DataTypes.STRING,
      allowNull: false,
    },
    roadmapDescription: {
      type: DataTypes.STRING,
      allowNull: false,
    },
    progress: {
      type: DataTypes.DECIMAL(5, 2),
      allowNull: false,
    },
    preAddress: {
      type: DataTypes.STRING,
      allowNull: false,
    },
    totalPlasticCredits: {
      type: DataTypes.INTEGER,
      allowNull: false,
    },
    soldPlasticCredits: {
      type: DataTypes.INTEGER,
      allowNull: false,
    },
    totalPlasticTokens: {
      type: DataTypes.INTEGER,
      allowNull: false,
    },
    sentPlasticTokens: {
      type: DataTypes.INTEGER,
      allowNull: false,
    },
    totalPlastic: {
      type: DataTypes.INTEGER,
      allowNull: false,
    },
    recoveredPlastic: {
      type: DataTypes.INTEGER,
      allowNull: false,
    },
    createdAt: {
      type: DataTypes.DATE,
      defaultValue: DataTypes.NOW,
    },
    dateArchived: {
      type: DataTypes.DATE,
      allowNull: false,
      defaultValue: DataTypes.NOW,
    },
    status: {
      type: DataTypes.STRING,
      allowNull: false,
      defaultValue: "archived",
    },
  },
  {
    sequelize,
    modelName: "ArchivedRoadmap",
    tableName: "archived_roadmaps",
    timestamps: false,
  }
);

export default ArchivedRoadmap;
