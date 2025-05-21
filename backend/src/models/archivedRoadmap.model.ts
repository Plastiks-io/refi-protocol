import { DataTypes, Model } from "sequelize";
import sequelize from "../db/config.js";

// Define the model for completed roadmaps
class ArchivedRoadmap extends Model {}

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
      type: DataTypes.INTEGER,
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
  },
  {
    sequelize,
    modelName: "ArchivedRoadmap",
    tableName: "archived_roadmaps",
    timestamps: false,
  }
);

export default ArchivedRoadmap;
