import { describe, it, expect, vi, beforeEach } from "vitest";
import ArchivedRoadmap from "../../src/models/archivedRoadmap.model.js";

describe("ArchivedRoadmap Model (Mocked)", () => {
  beforeEach(() => {
    vi.restoreAllMocks();
  });

  it("should create archived roadmap with all required fields", async () => {
    const roadmapData = {
      preId: "PRE001",
      roadmapId: "ROADMAP001",
      roadmapName: "Ocean Cleanup Initiative",
      roadmapDescription: "Clean plastic from ocean waters",
      progress: 100,
      preAddress: "0x1234567890abcdef",
      totalPlasticCredits: 1000,
      soldPlasticCredits: 800,
      totalPlasticTokens: 5000,
      sentPlasticTokens: 4000,
      totalPlastic: 10000,
      recoveredPlastic: 8500,
    };

    const mockResult = {
      id: "mock-uuid-1",
      ...roadmapData,
      status: "archived",
      createdAt: new Date(),
      dateArchived: new Date(),
    };

    vi.spyOn(ArchivedRoadmap, "create").mockResolvedValue(mockResult as any);

    const archivedRoadmap = await ArchivedRoadmap.create(roadmapData);

    expect(archivedRoadmap).toBeDefined();
    expect(archivedRoadmap.roadmapName).toBe(roadmapData.roadmapName);
    expect(archivedRoadmap.progress).toBe(roadmapData.progress);
    expect(archivedRoadmap.status).toBe("archived");
    expect(archivedRoadmap.createdAt).toBeInstanceOf(Date);
    expect(archivedRoadmap.dateArchived).toBeInstanceOf(Date);
  });

  it("should auto-generate UUID and default values", async () => {
    const mockResult = {
      id: "mock-uuid-2",
      status: "archived",
      createdAt: new Date(),
      dateArchived: new Date(),
    };

    vi.spyOn(ArchivedRoadmap, "create").mockResolvedValue(mockResult as any);

    const roadmap = await ArchivedRoadmap.create({
      preId: "PRE002",
      roadmapId: "ROADMAP002",
      roadmapName: "Beach Cleanup",
      roadmapDescription: "Clean plastic from beaches",
      progress: 75,
      preAddress: "0xabcdef1234567890",
      totalPlasticCredits: 500,
      soldPlasticCredits: 300,
      totalPlasticTokens: 2000,
      sentPlasticTokens: 1500,
      totalPlastic: 5000,
      recoveredPlastic: 3750,
    });

    expect(roadmap.id).toBeDefined();
    expect(roadmap.status).toBe("archived");
    expect(roadmap.createdAt).toBeInstanceOf(Date);
    expect(roadmap.dateArchived).toBeInstanceOf(Date);
  });

  it("should require all mandatory fields", async () => {
    vi.spyOn(ArchivedRoadmap, "create").mockRejectedValue(
      new Error("Missing required fields")
    );

    await expect(
      ArchivedRoadmap.create({
        preId: "PRE003",
      } as any)
    ).rejects.toThrow("Missing required fields");
  });

  it("should not allow null values for required fields", async () => {
    vi.spyOn(ArchivedRoadmap, "create").mockRejectedValue(
      new Error("Validation error: preId cannot be null")
    );

    await expect(
      ArchivedRoadmap.create({
        preId: null,
        roadmapId: "ROADMAP003",
        roadmapName: "Test Roadmap",
        roadmapDescription: "Test Description",
        progress: 50,
        preAddress: "0xtest123456",
        totalPlasticCredits: 100,
        soldPlasticCredits: 50,
        totalPlasticTokens: 500,
        sentPlasticTokens: 250,
        totalPlastic: 1000,
        recoveredPlastic: 500,
      } as any)
    ).rejects.toThrow("preId cannot be null");
  });

  it("should update archived roadmap fields", async () => {
    const roadmap = {
      progress: 60,
      recoveredPlastic: 1200,
      update: vi.fn().mockResolvedValue(undefined),
    };

    await roadmap.update({
      progress: 80,
      recoveredPlastic: 1600,
    });

    expect(roadmap.update).toHaveBeenCalledWith({
      progress: 80,
      recoveredPlastic: 1600,
    });
  });

  it("should handle integer fields correctly", async () => {
    const mockResult = {
      progress: 0,
      totalPlasticCredits: 0,
      recoveredPlastic: 0,
    };

    vi.spyOn(ArchivedRoadmap, "create").mockResolvedValue(mockResult as any);

    const roadmap = await ArchivedRoadmap.create({
      preId: "PRE007",
      roadmapId: "ROADMAP007",
      roadmapName: "Integer Test",
      roadmapDescription: "Test integer handling",
      progress: 0,
      preAddress: "0xinteger123456",
      totalPlasticCredits: 0,
      soldPlasticCredits: 0,
      totalPlasticTokens: 0,
      sentPlasticTokens: 0,
      totalPlastic: 0,
      recoveredPlastic: 0,
    });

    expect(roadmap.progress).toBe(0);
    expect(roadmap.totalPlasticCredits).toBe(0);
    expect(roadmap.recoveredPlastic).toBe(0);
  });

  it("should count archived roadmaps", async () => {
    vi.spyOn(ArchivedRoadmap, "bulkCreate").mockResolvedValue([] as any);
    vi.spyOn(ArchivedRoadmap, "count").mockResolvedValue(2);

    await ArchivedRoadmap.bulkCreate([
      {
        preId: "PRE009",
        roadmapId: "COUNT001",
        roadmapName: "Count Test 1",
        roadmapDescription: "First count test",
        progress: 25,
        preAddress: "0xcount1",
        totalPlasticCredits: 100,
        soldPlasticCredits: 25,
        totalPlasticTokens: 500,
        sentPlasticTokens: 125,
        totalPlastic: 1000,
        recoveredPlastic: 250,
      },
      {
        preId: "PRE010",
        roadmapId: "COUNT002",
        roadmapName: "Count Test 2",
        roadmapDescription: "Second count test",
        progress: 75,
        preAddress: "0xcount2",
        totalPlasticCredits: 200,
        soldPlasticCredits: 150,
        totalPlasticTokens: 1000,
        sentPlasticTokens: 750,
        totalPlastic: 2000,
        recoveredPlastic: 1500,
      },
    ]);

    const count = await ArchivedRoadmap.count();
    expect(count).toBe(2);
  });
});
