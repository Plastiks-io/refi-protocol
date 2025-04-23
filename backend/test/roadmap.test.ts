import {
  initializeRoadmap,
  updateRoadmap,
  getAllRoadmaps,
  releaseFunds,
  queryTransaction,
  queryAddressHistory,
  getAllCompletedRoadmaps,
} from "../src/controllers/roadmap.controller.js";
import { jest } from "@jest/globals";
import CompletedRoadmap from "../src/models/completedRoadmap.model.js";

describe("Roadmap Controller Tests", () => {
  let req, res;

  beforeEach(() => {
    req = {
      body: {},
      roadmap: {},
    };

    res = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn(),
    };

    jest.clearAllMocks();
    jest.spyOn(console, "error").mockImplementation(() => {});
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  describe("initializeRoadmap", () => {
    it("should return 400 if required fields are missing", async () => {
      await initializeRoadmap(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({
        error: "Missing required fields",
      });
    });

    it("should return 409 if roadmap already exists", async () => {
      req.body = {
        preId: "pre1",
        roadmapId: "roadmap1",
        roadmapName: "CleanCoast Mission",
        roadmapDescription:
          "Focused on removing plastic waste from coastal regions and supporting local recycling units.",
        prePkh: "b93e78824bcf5c34a62b2f573727b4bb8a1365ebd152bd6243ff8dc6",
        preSkh: "a786470d2a2c8bc00ecaf662a64407364be25325f33d1cb9446b4bd7",
        totalPlasticCredits: 100,
        totalPlasticTokens: 10000,
        totalPlastic: 100,
      };

      // ✅ Properly mock CompletedRoadmap.findOne
      jest
        .spyOn(CompletedRoadmap, "findOne")
        .mockResolvedValueOnce({ ...req.body });

      await initializeRoadmap(req, res);

      expect(res.status).toHaveBeenCalledWith(409);
      expect(res.json).toHaveBeenCalledWith({
        message: "Roadmap already exists in completed roadmaps",
        success: false,
      });
    });

    it("should return 200 and txHash on success", async () => {
      req.body = {
        preId: "pre8",
        roadmapId: "roadmap8",
        roadmapName: "CleanCoast Mission",
        roadmapDescription:
          "Focused on removing plastic waste from coastal regions and supporting local recycling units.",
        prePkh: "b93e78824bcf5c34a62b2f573727b4bb8a1365ebd152bd6243ff8dc6",
        preSkh: "a786470d2a2c8bc00ecaf662a64407364be25325f33d1cb9446b4bd7",
        totalPlasticCredits: 100,
        totalPlasticTokens: 10000,
        totalPlastic: 100,
      };

      // mock the initializeRoadmap to return a successful response when data is stored in smart contract

      await initializeRoadmap(req, res);
      expect(res.status).toHaveBeenCalledWith(200);
    });
  });
});
