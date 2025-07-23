// test/controllers/roadmap.controller.test.ts
import { describe, it, expect, vi, beforeEach } from "vitest";
import {
  initializeRoadmap,
  getAllCompletedRoadmaps,
  deleteArchivedRoadmap,
} from "../../src/controllers/roadmap.controller";
import { Request, Response } from "express";
import CompletedRoadmap from "../../src/models/completedRoadmap.model";
import ArchivedRoadmap from "../../src/models/archivedRoadmap.model";
import { Lucid } from "lucid-cardano";

vi.mock("lucid-cardano");
vi.mock("../../src/models/completedRoadmap.model");
vi.mock("../../src/models/archivedRoadmap.model");
vi.mock("../../src/contract/contracts.js", () => ({
  refiValidator: {},
}));
vi.mock("../../src/controllers/stakeReward.controller.js", () => ({
  getPubKeyHash: vi.fn().mockResolvedValue("dummyPkh"),
}));

const createMockRes = () => {
  const res = {} as Response;
  res.status = vi.fn().mockReturnThis();
  res.json = vi.fn();
  return res;
};

describe("initializeRoadmap", () => {
  it("should return 400 when required fields are missing", async () => {
    const req = {
      body: {
        preId: "abc",
        // roadmapId missing intentionally
      },
    } as Request;
    const res = createMockRes();

    await initializeRoadmap(req, res);

    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.json).toHaveBeenCalledWith({ error: "Missing required fields" });
  });

  it("should return 500 on Lucid error", async () => {
    const req = {
      body: {
        preId: "pre1",
        roadmapId: "rm1",
        roadmapName: "Test",
        roadmapDescription: "Test Desc",
        prePkh: "ppkh",
        preSkh: "pskh",
        totalPlasticCredits: 100,
      },
    } as Request;
    const res = createMockRes();

    (CompletedRoadmap.findOne as any).mockResolvedValue(null);
    (ArchivedRoadmap.findOne as any).mockResolvedValue(null);
    (Lucid.new as any).mockRejectedValue(new Error("Lucid init error"));

    await initializeRoadmap(req, res);

    expect(res.status).toHaveBeenCalledWith(500);
    expect(res.json).toHaveBeenCalledWith(
      expect.objectContaining({
        success: false,
        message: "Error occured during the creation of roadmap",
      })
    );
  });
});

describe("getAllCompletedRoadmaps", () => {
  it("should return 200 with empty array when no roadmaps exist", async () => {
    const req = {} as Request;
    const res = createMockRes();

    (CompletedRoadmap.findAll as any).mockResolvedValue([]);

    await getAllCompletedRoadmaps(req, res);

    expect(res.status).toHaveBeenCalledWith(200);
    expect(res.json).toHaveBeenCalledWith({
      message: "No completed roadmaps found",
      roadmaps: [],
    });
  });

  it("should return 200 with roadmap list", async () => {
    const mockData = [{ id: 1, preId: "p1" }];
    (CompletedRoadmap.findAll as any).mockResolvedValue(mockData);

    const req = {} as Request;
    const res = createMockRes();

    await getAllCompletedRoadmaps(req, res);

    expect(res.status).toHaveBeenCalledWith(200);
    expect(res.json).toHaveBeenCalledWith({
      message: "Completed roadmaps retrieved successfully",
      roadmaps: mockData,
    });
  });
});

describe("deleteArchivedRoadmap", () => {
  it("should return 404 if archived roadmap not found", async () => {
    const req = {
      params: { id: "123" },
    } as unknown as Request;
    const res = createMockRes();

    (ArchivedRoadmap.findOne as any).mockResolvedValue(null);

    await deleteArchivedRoadmap(req, res);

    expect(res.status).toHaveBeenCalledWith(404);
    expect(res.json).toHaveBeenCalledWith({
      error: "Archived roadmap not found",
    });
  });

  it("should delete roadmap and return 200", async () => {
    const destroyMock = vi.fn();
    const req = {
      params: { id: "123" },
    } as unknown as Request;
    const res = createMockRes();

    (ArchivedRoadmap.findOne as any).mockResolvedValue({
      destroy: destroyMock,
    });

    await deleteArchivedRoadmap(req, res);

    expect(destroyMock).toHaveBeenCalled();
    expect(res.status).toHaveBeenCalledWith(200);
    expect(res.json).toHaveBeenCalledWith({
      message: "Archived roadmap deleted successfully",
      success: true,
    });
  });
});
