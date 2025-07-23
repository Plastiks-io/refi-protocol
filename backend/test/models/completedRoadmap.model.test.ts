import { describe, it, expect, vi, beforeEach } from "vitest";
import CompletedRoadmap from "../../src/models/completedRoadmap.model";

describe("CompletedRoadmap model (mocked) tests", () => {
  beforeEach(() => {
    vi.restoreAllMocks();
  });

  it("should create a CompletedRoadmap instance with valid fields", async () => {
    const fakeInstance = {
      id: "mock-uuid",
      status: "completed",
      createdAt: new Date(),
    };

    vi.spyOn(CompletedRoadmap, "create").mockResolvedValue(fakeInstance as any);

    const roadmap = await CompletedRoadmap.create({
      preId: "PRE123",
      roadmapId: "RM001",
      roadmapName: "Ocean Cleanup",
      roadmapDescription: "Clean the ocean in 30 days",
      progress: 100,
      preAddress: "0xabc123",
      totalPlasticCredits: 500,
      soldPlasticCredits: 300,
      totalPlasticTokens: 1000,
      sentPlasticTokens: 500,
      totalPlastic: 200,
      recoveredPlastic: 150,
    });

    expect(roadmap.id).toBe("mock-uuid");
    expect(roadmap.status).toBe("completed");
    expect(roadmap.createdAt).toBeInstanceOf(Date);
  });

  it("should throw error if required fields are missing", async () => {
    vi.spyOn(CompletedRoadmap, "create").mockRejectedValue(
      new Error("Missing required field: preId")
    );

    await expect(
      CompletedRoadmap.create({
        roadmapId: "RM002",
        roadmapName: "No PreID",
        roadmapDescription: "Missing preId",
        progress: 50,
        preAddress: "0xdef456",
        totalPlasticCredits: 100,
        soldPlasticCredits: 50,
        totalPlasticTokens: 200,
        sentPlasticTokens: 100,
        totalPlastic: 80,
        recoveredPlastic: 60,
      } as any)
    ).rejects.toThrow("Missing required field: preId");
  });

  it("should throw error if numeric field gets a string", async () => {
    vi.spyOn(CompletedRoadmap, "create").mockRejectedValue(
      new Error("Invalid value for numeric field: progress")
    );

    await expect(
      CompletedRoadmap.create({
        preId: "PRE124",
        roadmapId: "RM003",
        roadmapName: "Bad Progress",
        roadmapDescription: "Invalid progress type",
        progress: "not a number" as any,
        preAddress: "0xghi789",
        totalPlasticCredits: 100,
        soldPlasticCredits: 50,
        totalPlasticTokens: 200,
        sentPlasticTokens: 100,
        totalPlastic: 80,
        recoveredPlastic: 60,
      })
    ).rejects.toThrow("Invalid value for numeric field: progress");
  });

  it('should set status to "completed" by default', async () => {
    const fakeInstance = {
      status: "completed",
    };
    vi.spyOn(CompletedRoadmap, "create").mockResolvedValue(fakeInstance as any);

    const roadmap = await CompletedRoadmap.create({
      preId: "PRE125",
      roadmapId: "RM004",
      roadmapName: "Auto Status",
      roadmapDescription: "Status default test",
      progress: 75,
      preAddress: "0xjkl012",
      totalPlasticCredits: 300,
      soldPlasticCredits: 150,
      totalPlasticTokens: 600,
      sentPlasticTokens: 300,
      totalPlastic: 120,
      recoveredPlastic: 90,
    });

    expect(roadmap.status).toBe("completed");
  });

  it("should allow negative values (if no validation)", async () => {
    const fakeInstance = {
      progress: -10,
      totalPlasticCredits: -100,
    };
    vi.spyOn(CompletedRoadmap, "create").mockResolvedValue(fakeInstance as any);

    const roadmap = await CompletedRoadmap.create({
      preId: "PRE126",
      roadmapId: "RM005",
      roadmapName: "Negative Values",
      roadmapDescription: "Testing negative input",
      progress: -10,
      preAddress: "0xmno345",
      totalPlasticCredits: -100,
      soldPlasticCredits: -50,
      totalPlasticTokens: -200,
      sentPlasticTokens: -100,
      totalPlastic: -80,
      recoveredPlastic: -60,
    });

    expect(roadmap.progress).toBeLessThan(0);
    expect(roadmap.totalPlasticCredits).toBeLessThan(0);
  });
});
